    // ===== CRYPTO MODULE =====
    // Zig native: src/polyfills/crypto.zig (hash, hmac, aesGcmEncrypt/Decrypt, randomBytes, randomUUID)
    // JS: thin wrapper with createHash/createHmac object interfaces
    {
        const _crypto = _modules.crypto || globalThis.crypto;

        // Wrap native randomBytes to return Buffer
        const nativeRandomBytes = _crypto?.randomBytes;
        const randomBytes = nativeRandomBytes
            ? (size) => Buffer.from(nativeRandomBytes(size))
            : null;

        // Use native randomUUID or wrap randomBytes
        const randomUUID = _crypto?.randomUUID || null;

        _modules.crypto = {
            // Cryptographically secure random - delegates to Zig std.crypto.random
            // Supports both sync (returns Buffer) and callback (async) patterns
            randomBytes: function(size, callback) {
                const bytes = randomBytes ? randomBytes(size) : null;
                if (!bytes && !callback) throw new Error('crypto.randomBytes not available - Zig native not registered');
                if (callback) {
                    if (bytes) {
                        setImmediate(() => callback(null, bytes));
                    } else {
                        setImmediate(() => callback(new Error('crypto.randomBytes not available')));
                    }
                    return;
                }
                return bytes;
            },
            // Sync-only version for compatibility
            randomBytesSync: randomBytes || function(size) {
                throw new Error('crypto.randomBytes not available - Zig native not registered');
            },
            randomUUID: randomUUID || function() {
                throw new Error('crypto.randomUUID not available - Zig native not registered');
            },
            randomInt: _crypto?.randomInt || function() {
                throw new Error('crypto.randomInt not available - Zig native not registered');
            },
            timingSafeEqual: _crypto?.timingSafeEqual || function(a, b) {
                throw new Error('crypto.timingSafeEqual not available - Zig native not registered');
            },
            pbkdf2Sync: function(password, salt, iterations, keylen, digest) {
                const result = _crypto?.pbkdf2Sync?.(password, salt, iterations, keylen, digest);
                if (!result) throw new Error('crypto.pbkdf2Sync not available - Zig native not registered');
                return Buffer.from(result);
            },
            pbkdf2: function(password, salt, iterations, keylen, digest, callback) {
                try {
                    const result = _modules.crypto.pbkdf2Sync(password, salt, iterations, keylen, digest);
                    if (callback) setTimeout(() => callback(null, result), 0);
                } catch (e) {
                    if (callback) setTimeout(() => callback(e), 0);
                }
            },
            getHashes: () => ['sha256', 'sha384', 'sha512', 'sha1', 'md5'],
            getCiphers: () => ['aes-256-gcm', 'aes-256-cbc', 'aes-128-cbc'],

            // createHash - wrapper that calls Zig hash on digest()
            // Node.js signature: createHash(algorithm[, options])
            createHash: function(algorithm, options) {
                const algo = algorithm.toLowerCase();
                let data = '';
                return {
                    // Node.js signature: update(data[, inputEncoding])
                    update(input, inputEncoding) {
                        if (typeof input === 'string') {
                            // Handle different input encodings
                            if (inputEncoding === 'hex') {
                                // Convert hex to binary string
                                for (let i = 0; i < input.length; i += 2) {
                                    data += String.fromCharCode(parseInt(input.slice(i, i + 2), 16));
                                }
                            } else if (inputEncoding === 'base64') {
                                data += atob(input);
                            } else {
                                // utf8 or no encoding
                                data += input;
                            }
                        } else if (Buffer.isBuffer(input) || input instanceof Uint8Array) {
                            data += String.fromCharCode.apply(null, input);
                        } else {
                            data += String(input);
                        }
                        return this;
                    },
                    digest(encoding) {
                        const result = _crypto.hash?.(algo, data) ?? globalThis.__edgebox_hash?.(algo, data);
                        if (!result) throw new Error('hash not available');
                        if (encoding === 'hex') return result;
                        if (encoding === 'base64') {
                            const bytes = [];
                            for (let i = 0; i < result.length; i += 2) bytes.push(parseInt(result.slice(i, i + 2), 16));
                            return btoa(String.fromCharCode.apply(null, bytes));
                        }
                        const bytes = [];
                        for (let i = 0; i < result.length; i += 2) bytes.push(parseInt(result.slice(i, i + 2), 16));
                        return Buffer.from(bytes);
                    },
                    copy() {
                        // Return a new hash with the same state
                        const newHash = _modules.crypto.createHash(algorithm, options);
                        newHash._data = data;
                        return newHash;
                    }
                };
            },

            // createHmac - wrapper that calls Zig hmac on digest()
            // Node.js signature: createHmac(algorithm, key[, options])
            createHmac: function(algorithm, key, options) {
                const algo = algorithm.toLowerCase();
                const keyStr = typeof key === 'string' ? key : String.fromCharCode.apply(null, key);
                let data = '';
                return {
                    // Node.js signature: update(data[, inputEncoding])
                    update(input, inputEncoding) {
                        if (typeof input === 'string') {
                            if (inputEncoding === 'hex') {
                                for (let i = 0; i < input.length; i += 2) {
                                    data += String.fromCharCode(parseInt(input.slice(i, i + 2), 16));
                                }
                            } else if (inputEncoding === 'base64') {
                                data += atob(input);
                            } else {
                                data += input;
                            }
                        } else if (Buffer.isBuffer(input) || input instanceof Uint8Array) {
                            data += String.fromCharCode.apply(null, input);
                        } else {
                            data += String(input);
                        }
                        return this;
                    },
                    digest(encoding) {
                        const result = _crypto.hmac?.(algo, keyStr, data) ?? globalThis.__edgebox_hmac?.(algo, keyStr, data);
                        if (!result) throw new Error('hmac not available');
                        if (encoding === 'hex') return result;
                        if (encoding === 'base64') {
                            const bytes = [];
                            for (let i = 0; i < result.length; i += 2) bytes.push(parseInt(result.slice(i, i + 2), 16));
                            return btoa(String.fromCharCode.apply(null, bytes));
                        }
                        const bytes = [];
                        for (let i = 0; i < result.length; i += 2) bytes.push(parseInt(result.slice(i, i + 2), 16));
                        return Buffer.from(bytes);
                    }
                };
            },

            // createCipheriv - streaming cipher interface using native AES
            createCipheriv: function(algorithm, key, iv, options) {
                const algo = algorithm.toLowerCase();
                const keyBuf = Buffer.isBuffer(key) ? key : Buffer.from(key);
                const ivBuf = Buffer.isBuffer(iv) ? iv : Buffer.from(iv);

                // Validate algorithm and key sizes
                if (algo === 'aes-256-cbc') {
                    if (keyBuf.length !== 32) throw new Error('Invalid key length for aes-256-cbc (need 32 bytes)');
                    if (ivBuf.length !== 16) throw new Error('Invalid IV length (need 16 bytes)');
                } else if (algo === 'aes-128-cbc') {
                    if (keyBuf.length !== 16) throw new Error('Invalid key length for aes-128-cbc (need 16 bytes)');
                    if (ivBuf.length !== 16) throw new Error('Invalid IV length (need 16 bytes)');
                } else if (algo === 'aes-256-gcm' || algo === 'aes-128-gcm') {
                    throw new Error(algo + ' not yet supported via createCipheriv - use aesGcmEncrypt directly');
                } else {
                    throw new Error('Unsupported algorithm: ' + algo + ' (supported: aes-256-cbc, aes-128-cbc)');
                }

                let buffer = Buffer.alloc(0);
                let finalized = false;

                return {
                    update(data, inputEncoding, outputEncoding) {
                        if (finalized) throw new Error('Cipher already finalized');
                        const dataBuf = Buffer.isBuffer(data) ? data :
                            inputEncoding === 'hex' ? Buffer.from(data, 'hex') :
                            inputEncoding === 'base64' ? Buffer.from(data, 'base64') :
                            Buffer.from(data, inputEncoding || 'utf8');
                        buffer = Buffer.concat([buffer, dataBuf]);
                        // Return empty buffer for streaming - all data returned in final()
                        const result = Buffer.alloc(0);
                        if (outputEncoding === 'hex') return result.toString('hex');
                        if (outputEncoding === 'base64') return result.toString('base64');
                        return result;
                    },
                    final(outputEncoding) {
                        if (finalized) throw new Error('Cipher already finalized');
                        finalized = true;
                        // Call native aesCbcEncrypt
                        const result = _crypto.aesCbcEncrypt(keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.length),
                                                            ivBuf.buffer.slice(ivBuf.byteOffset, ivBuf.byteOffset + ivBuf.length),
                                                            buffer.buffer.slice(buffer.byteOffset, buffer.byteOffset + buffer.length));
                        const outBuf = Buffer.from(result);
                        if (outputEncoding === 'hex') return outBuf.toString('hex');
                        if (outputEncoding === 'base64') return outBuf.toString('base64');
                        return outBuf;
                    },
                    setAutoPadding(autoPadding) { return this; }, // PKCS7 padding always enabled
                    getAuthTag() { throw new Error('getAuthTag only available for GCM mode'); },
                    setAAD(buffer) { throw new Error('setAAD only available for GCM mode'); }
                };
            },

            createDecipheriv: function(algorithm, key, iv, options) {
                const algo = algorithm.toLowerCase();
                const keyBuf = Buffer.isBuffer(key) ? key : Buffer.from(key);
                const ivBuf = Buffer.isBuffer(iv) ? iv : Buffer.from(iv);

                // Validate algorithm and key sizes
                if (algo === 'aes-256-cbc') {
                    if (keyBuf.length !== 32) throw new Error('Invalid key length for aes-256-cbc (need 32 bytes)');
                    if (ivBuf.length !== 16) throw new Error('Invalid IV length (need 16 bytes)');
                } else if (algo === 'aes-128-cbc') {
                    if (keyBuf.length !== 16) throw new Error('Invalid key length for aes-128-cbc (need 16 bytes)');
                    if (ivBuf.length !== 16) throw new Error('Invalid IV length (need 16 bytes)');
                } else if (algo === 'aes-256-gcm' || algo === 'aes-128-gcm') {
                    throw new Error(algo + ' not yet supported via createDecipheriv - use aesGcmDecrypt directly');
                } else {
                    throw new Error('Unsupported algorithm: ' + algo + ' (supported: aes-256-cbc, aes-128-cbc)');
                }

                let buffer = Buffer.alloc(0);
                let finalized = false;

                return {
                    update(data, inputEncoding, outputEncoding) {
                        if (finalized) throw new Error('Decipher already finalized');
                        const dataBuf = Buffer.isBuffer(data) ? data :
                            inputEncoding === 'hex' ? Buffer.from(data, 'hex') :
                            inputEncoding === 'base64' ? Buffer.from(data, 'base64') :
                            Buffer.from(data, inputEncoding || 'binary');
                        buffer = Buffer.concat([buffer, dataBuf]);
                        // Return empty buffer for streaming - all data returned in final()
                        const result = Buffer.alloc(0);
                        if (outputEncoding === 'hex') return result.toString('hex');
                        if (outputEncoding === 'base64') return result.toString('base64');
                        if (outputEncoding === 'utf8' || outputEncoding === 'utf-8') return result.toString('utf8');
                        return result;
                    },
                    final(outputEncoding) {
                        if (finalized) throw new Error('Decipher already finalized');
                        finalized = true;
                        // Call native aesCbcDecrypt
                        const result = _crypto.aesCbcDecrypt(keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.length),
                                                            ivBuf.buffer.slice(ivBuf.byteOffset, ivBuf.byteOffset + ivBuf.length),
                                                            buffer.buffer.slice(buffer.byteOffset, buffer.byteOffset + buffer.length));
                        const outBuf = Buffer.from(result);
                        if (outputEncoding === 'hex') return outBuf.toString('hex');
                        if (outputEncoding === 'base64') return outBuf.toString('base64');
                        if (outputEncoding === 'utf8' || outputEncoding === 'utf-8') return outBuf.toString('utf8');
                        return outBuf;
                    },
                    setAutoPadding(autoPadding) { return this; }, // PKCS7 padding always enabled
                    setAuthTag(tag) { throw new Error('setAuthTag only available for GCM mode'); },
                    setAAD(buffer) { throw new Error('setAAD only available for GCM mode'); }
                };
            },

            createCipher: function(algorithm, password, options) {
                throw new Error('createCipher is deprecated - use createCipheriv instead');
            },
            createDecipher: function(algorithm, password, options) {
                throw new Error('createDecipher is deprecated - use createDecipheriv instead');
            },

            // Signing stubs
            createSign: function(algorithm, options) {
                throw new Error('createSign not implemented');
            },
            createVerify: function(algorithm, options) {
                throw new Error('createVerify not implemented');
            },
            sign: function(algorithm, data, key, callback) {
                throw new Error('crypto.sign not implemented');
            },
            verify: function(algorithm, data, key, signature, callback) {
                throw new Error('crypto.verify not implemented');
            },

            // Key derivation - delegates to native Zig
            scrypt: function(password, salt, keylen, options, callback) {
                if (typeof options === 'function') { callback = options; options = {}; }
                try {
                    const result = _modules.crypto.scryptSync(password, salt, keylen, options);
                    if (callback) setTimeout(() => callback(null, result), 0);
                } catch (e) {
                    if (callback) setTimeout(() => callback(e), 0);
                }
            },
            scryptSync: function(password, salt, keylen, options) {
                const result = _crypto?.scryptSync?.(password, salt, keylen, options);
                if (!result) throw new Error('scryptSync not available - Zig native not registered');
                return Buffer.from(result);
            },
            hkdf: function(digest, ikm, salt, info, keylen, callback) {
                try {
                    const result = _modules.crypto.hkdfSync(digest, ikm, salt, info, keylen);
                    if (callback) setTimeout(() => callback(null, result), 0);
                } catch (e) {
                    if (callback) setTimeout(() => callback(e), 0);
                }
            },
            hkdfSync: function(digest, ikm, salt, info, keylen) {
                const result = _crypto?.hkdfSync?.(digest, ikm, salt, info, keylen);
                if (!result) throw new Error('hkdfSync not available - Zig native not registered');
                return Buffer.from(result);
            },

            // Key generation stubs
            generateKeyPair: function(type, options, callback) {
                if (typeof options === 'function') { callback = options; options = {}; }
                if (callback) setTimeout(() => callback(new Error('generateKeyPair not implemented')), 0);
            },
            generateKeyPairSync: function(type, options) {
                throw new Error('generateKeyPairSync not implemented');
            },
            generateKey: function(type, options, callback) {
                if (typeof options === 'function') { callback = options; options = {}; }
                if (callback) setTimeout(() => callback(new Error('generateKey not implemented')), 0);
            },
            generateKeySync: function(type, options) {
                throw new Error('generateKeySync not implemented');
            },

            // Key object stubs
            createPrivateKey: function(key) {
                throw new Error('createPrivateKey not implemented');
            },
            createPublicKey: function(key) {
                throw new Error('createPublicKey not implemented');
            },
            createSecretKey: function(key, encoding) {
                throw new Error('createSecretKey not implemented');
            },

            // DiffieHellman stubs
            createDiffieHellman: function(prime, primeEncoding, generator, generatorEncoding) {
                throw new Error('createDiffieHellman not implemented');
            },
            createDiffieHellmanGroup: function(name) {
                throw new Error('createDiffieHellmanGroup not implemented');
            },
            createECDH: function(curveName) {
                throw new Error('createECDH not implemented');
            },
            getDiffieHellman: function(groupName) {
                throw new Error('getDiffieHellman not implemented');
            },

            // Crypto constants
            constants: {
                OPENSSL_VERSION_NUMBER: 0,
                SSL_OP_ALL: 0,
                SSL_OP_NO_SSLv2: 0x01000000,
                SSL_OP_NO_SSLv3: 0x02000000,
                SSL_OP_NO_TLSv1: 0x04000000,
                SSL_OP_NO_TLSv1_1: 0x10000000,
                SSL_OP_NO_TLSv1_2: 0x08000000,
                RSA_PKCS1_PADDING: 1,
                RSA_SSLV23_PADDING: 2,
                RSA_NO_PADDING: 3,
                RSA_PKCS1_OAEP_PADDING: 4,
                RSA_X931_PADDING: 5,
                RSA_PKCS1_PSS_PADDING: 6,
                RSA_PSS_SALTLEN_DIGEST: -1,
                RSA_PSS_SALTLEN_MAX_SIGN: -2,
                RSA_PSS_SALTLEN_AUTO: -2,
                POINT_CONVERSION_COMPRESSED: 2,
                POINT_CONVERSION_UNCOMPRESSED: 4,
                POINT_CONVERSION_HYBRID: 6
            },

            // WebCrypto stub
            webcrypto: typeof globalThis.crypto !== 'undefined' ? globalThis.crypto : undefined
        };
    }
    _modules['node:crypto'] = _modules.crypto;

