    // ===== CRYPTO MODULE =====
    // Zig native: src/polyfills/crypto.zig (hash, hmac, aesGcmEncrypt/Decrypt, randomBytes, randomUUID)
    // JS: thin wrapper with createHash/createHmac object interfaces
    // Note: PEM to DER conversion is handled in Zig - JS just passes key data as-is
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
            getHashes: () => [
                'sha256', 'sha384', 'sha512', 'sha1', 'md5',
                'sha3-256', 'sha3-384', 'sha3-512',
                'blake2b256', 'blake2b512', 'blake2s256',
                'blake2b-256', 'blake2b-512', 'blake2s-256'
            ],
            getCiphers: () => [
                'aes-256-gcm', 'aes-128-gcm',
                'aes-256-cbc', 'aes-128-cbc',
                'aes-256-ctr', 'aes-128-ctr'
            ],
            getCurves: () => [
                'prime256v1', 'P-256', 'secp256r1',  // P-256 ECDSA/ECDH
                'secp384r1', 'P-384',               // P-384 ECDSA/ECDH
                'x25519',                            // X25519 ECDH
                'ed25519'                            // Ed25519 signatures
            ],
            getCipherInfo: function(nameOrNid, options) {
                const ciphers = {
                    'aes-256-gcm': { name: 'aes-256-gcm', nid: 901, blockSize: 1, ivLength: 12, keyLength: 32, mode: 'gcm' },
                    'aes-128-gcm': { name: 'aes-128-gcm', nid: 895, blockSize: 1, ivLength: 12, keyLength: 16, mode: 'gcm' },
                    'aes-256-cbc': { name: 'aes-256-cbc', nid: 427, blockSize: 16, ivLength: 16, keyLength: 32, mode: 'cbc' },
                    'aes-128-cbc': { name: 'aes-128-cbc', nid: 419, blockSize: 16, ivLength: 16, keyLength: 16, mode: 'cbc' },
                    'aes-256-ctr': { name: 'aes-256-ctr', nid: 906, blockSize: 1, ivLength: 16, keyLength: 32, mode: 'ctr' },
                    'aes-128-ctr': { name: 'aes-128-ctr', nid: 904, blockSize: 1, ivLength: 16, keyLength: 16, mode: 'ctr' }
                };
                const name = typeof nameOrNid === 'string' ? nameOrNid.toLowerCase() : null;
                if (name && ciphers[name]) {
                    return ciphers[name];
                }
                // Search by NID
                if (typeof nameOrNid === 'number') {
                    for (const cipher of Object.values(ciphers)) {
                        if (cipher.nid === nameOrNid) return cipher;
                    }
                }
                return undefined;
            },

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
                let isCtr = false;
                let isGcm = false;

                // Validate algorithm and key sizes
                if (algo === 'aes-256-cbc') {
                    if (keyBuf.length !== 32) throw new Error('Invalid key length for aes-256-cbc (need 32 bytes)');
                    if (ivBuf.length !== 16) throw new Error('Invalid IV length (need 16 bytes)');
                } else if (algo === 'aes-128-cbc') {
                    if (keyBuf.length !== 16) throw new Error('Invalid key length for aes-128-cbc (need 16 bytes)');
                    if (ivBuf.length !== 16) throw new Error('Invalid IV length (need 16 bytes)');
                } else if (algo === 'aes-256-ctr') {
                    if (keyBuf.length !== 32) throw new Error('Invalid key length for aes-256-ctr (need 32 bytes)');
                    if (ivBuf.length !== 16) throw new Error('Invalid IV length (need 16 bytes)');
                    isCtr = true;
                } else if (algo === 'aes-128-ctr') {
                    if (keyBuf.length !== 16) throw new Error('Invalid key length for aes-128-ctr (need 16 bytes)');
                    if (ivBuf.length !== 16) throw new Error('Invalid IV length (need 16 bytes)');
                    isCtr = true;
                } else if (algo === 'aes-256-gcm') {
                    if (keyBuf.length !== 32) throw new Error('Invalid key length for aes-256-gcm (need 32 bytes)');
                    if (ivBuf.length !== 12) throw new Error('Invalid IV length for GCM (need 12 bytes)');
                    isGcm = true;
                } else if (algo === 'aes-128-gcm') {
                    if (keyBuf.length !== 16) throw new Error('Invalid key length for aes-128-gcm (need 16 bytes)');
                    if (ivBuf.length !== 12) throw new Error('Invalid IV length for GCM (need 12 bytes)');
                    isGcm = true;
                } else {
                    throw new Error('Unsupported algorithm: ' + algo + ' (supported: aes-256-cbc, aes-128-cbc, aes-256-ctr, aes-128-ctr, aes-256-gcm, aes-128-gcm)');
                }

                let buffer = Buffer.alloc(0);
                let finalized = false;
                let aadData = Buffer.alloc(0);
                let authTag = null;

                // For GCM mode, return special cipher object
                if (isGcm) {
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
                            // Call native GCM encrypt function (use AES-256 key padding if needed for 128)
                            let actualKey = keyBuf;
                            if (algo === 'aes-128-gcm') {
                                // Pad 16-byte key to 32 bytes for native aesGcmEncrypt
                                actualKey = Buffer.alloc(32);
                                keyBuf.copy(actualKey, 0, 0, 16);
                                keyBuf.copy(actualKey, 16, 0, 16); // Duplicate for padding
                            }
                            // Use native aesGcmEncrypt which returns ciphertext + 16-byte tag
                            const resultWithTag = _crypto.aesGcmEncrypt(
                                actualKey.buffer.slice(actualKey.byteOffset, actualKey.byteOffset + actualKey.length),
                                ivBuf.buffer.slice(ivBuf.byteOffset, ivBuf.byteOffset + ivBuf.length),
                                buffer.buffer.slice(buffer.byteOffset, buffer.byteOffset + buffer.length),
                                aadData.length > 0 ? aadData.buffer.slice(aadData.byteOffset, aadData.byteOffset + aadData.length) : undefined
                            );
                            const resultBuf = Buffer.from(resultWithTag);
                            // Split: ciphertext is all but last 16 bytes, tag is last 16 bytes
                            const ciphertext = resultBuf.slice(0, resultBuf.length - 16);
                            authTag = resultBuf.slice(resultBuf.length - 16);
                            if (outputEncoding === 'hex') return ciphertext.toString('hex');
                            if (outputEncoding === 'base64') return ciphertext.toString('base64');
                            return ciphertext;
                        },
                        setAutoPadding(autoPadding) { return this; },
                        getAuthTag() {
                            if (!finalized) throw new Error('Cannot get auth tag before calling final()');
                            if (!authTag) throw new Error('Auth tag not available');
                            return authTag;
                        },
                        setAAD(data, options) {
                            if (finalized) throw new Error('Cannot set AAD after calling final()');
                            const dataBuf = Buffer.isBuffer(data) ? data : Buffer.from(data);
                            aadData = Buffer.concat([aadData, dataBuf]);
                            return this;
                        }
                    };
                }

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
                        // Call native encrypt function
                        const encryptFn = isCtr ? _crypto.aesCtrEncrypt : _crypto.aesCbcEncrypt;
                        const result = encryptFn(keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.length),
                                                 ivBuf.buffer.slice(ivBuf.byteOffset, ivBuf.byteOffset + ivBuf.length),
                                                 buffer.buffer.slice(buffer.byteOffset, buffer.byteOffset + buffer.length));
                        const outBuf = Buffer.from(result);
                        if (outputEncoding === 'hex') return outBuf.toString('hex');
                        if (outputEncoding === 'base64') return outBuf.toString('base64');
                        return outBuf;
                    },
                    setAutoPadding(autoPadding) { return this; }, // PKCS7 padding for CBC, no padding for CTR
                    getAuthTag() { throw new Error('getAuthTag only available for GCM mode'); },
                    setAAD(buffer) { throw new Error('setAAD only available for GCM mode'); }
                };
            },

            createDecipheriv: function(algorithm, key, iv, options) {
                const algo = algorithm.toLowerCase();
                const keyBuf = Buffer.isBuffer(key) ? key : Buffer.from(key);
                const ivBuf = Buffer.isBuffer(iv) ? iv : Buffer.from(iv);
                let isCtr = false;
                let isGcm = false;

                // Validate algorithm and key sizes
                if (algo === 'aes-256-cbc') {
                    if (keyBuf.length !== 32) throw new Error('Invalid key length for aes-256-cbc (need 32 bytes)');
                    if (ivBuf.length !== 16) throw new Error('Invalid IV length (need 16 bytes)');
                } else if (algo === 'aes-128-cbc') {
                    if (keyBuf.length !== 16) throw new Error('Invalid key length for aes-128-cbc (need 16 bytes)');
                    if (ivBuf.length !== 16) throw new Error('Invalid IV length (need 16 bytes)');
                } else if (algo === 'aes-256-ctr') {
                    if (keyBuf.length !== 32) throw new Error('Invalid key length for aes-256-ctr (need 32 bytes)');
                    if (ivBuf.length !== 16) throw new Error('Invalid IV length (need 16 bytes)');
                    isCtr = true;
                } else if (algo === 'aes-128-ctr') {
                    if (keyBuf.length !== 16) throw new Error('Invalid key length for aes-128-ctr (need 16 bytes)');
                    if (ivBuf.length !== 16) throw new Error('Invalid IV length (need 16 bytes)');
                    isCtr = true;
                } else if (algo === 'aes-256-gcm') {
                    if (keyBuf.length !== 32) throw new Error('Invalid key length for aes-256-gcm (need 32 bytes)');
                    if (ivBuf.length !== 12) throw new Error('Invalid IV length for GCM (need 12 bytes)');
                    isGcm = true;
                } else if (algo === 'aes-128-gcm') {
                    if (keyBuf.length !== 16) throw new Error('Invalid key length for aes-128-gcm (need 16 bytes)');
                    if (ivBuf.length !== 12) throw new Error('Invalid IV length for GCM (need 12 bytes)');
                    isGcm = true;
                } else {
                    throw new Error('Unsupported algorithm: ' + algo + ' (supported: aes-256-cbc, aes-128-cbc, aes-256-ctr, aes-128-ctr, aes-256-gcm, aes-128-gcm)');
                }

                let buffer = Buffer.alloc(0);
                let finalized = false;
                let aadData = Buffer.alloc(0);
                let authTag = null;

                // For GCM mode, return special decipher object
                if (isGcm) {
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
                            if (!authTag) throw new Error('Auth tag required for GCM decryption - call setAuthTag() first');
                            finalized = true;
                            // Combine ciphertext + tag for native aesGcmDecrypt
                            const tagBuf = Buffer.isBuffer(authTag) ? authTag : Buffer.from(authTag);
                            const ciphertextWithTag = Buffer.concat([buffer, tagBuf]);
                            // Call native GCM decrypt function
                            let actualKey = keyBuf;
                            if (algo === 'aes-128-gcm') {
                                // Pad 16-byte key to 32 bytes for native aesGcmDecrypt
                                actualKey = Buffer.alloc(32);
                                keyBuf.copy(actualKey, 0, 0, 16);
                                keyBuf.copy(actualKey, 16, 0, 16);
                            }
                            const result = _crypto.aesGcmDecrypt(
                                actualKey.buffer.slice(actualKey.byteOffset, actualKey.byteOffset + actualKey.length),
                                ivBuf.buffer.slice(ivBuf.byteOffset, ivBuf.byteOffset + ivBuf.length),
                                ciphertextWithTag.buffer.slice(ciphertextWithTag.byteOffset, ciphertextWithTag.byteOffset + ciphertextWithTag.length),
                                aadData.length > 0 ? aadData.buffer.slice(aadData.byteOffset, aadData.byteOffset + aadData.length) : undefined
                            );
                            const outBuf = Buffer.from(result);
                            if (outputEncoding === 'hex') return outBuf.toString('hex');
                            if (outputEncoding === 'base64') return outBuf.toString('base64');
                            if (outputEncoding === 'utf8' || outputEncoding === 'utf-8') return outBuf.toString('utf8');
                            return outBuf;
                        },
                        setAutoPadding(autoPadding) { return this; },
                        setAuthTag(tag) {
                            if (finalized) throw new Error('Cannot set auth tag after calling final()');
                            authTag = Buffer.isBuffer(tag) ? tag : Buffer.from(tag);
                            return this;
                        },
                        setAAD(data, options) {
                            if (finalized) throw new Error('Cannot set AAD after calling final()');
                            const dataBuf = Buffer.isBuffer(data) ? data : Buffer.from(data);
                            aadData = Buffer.concat([aadData, dataBuf]);
                            return this;
                        }
                    };
                }

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
                        // Call native decrypt function (CTR uses same function for encrypt/decrypt)
                        const decryptFn = isCtr ? _crypto.aesCtrEncrypt : _crypto.aesCbcDecrypt;
                        const result = decryptFn(keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.length),
                                                 ivBuf.buffer.slice(ivBuf.byteOffset, ivBuf.byteOffset + ivBuf.length),
                                                 buffer.buffer.slice(buffer.byteOffset, buffer.byteOffset + buffer.length));
                        const outBuf = Buffer.from(result);
                        if (outputEncoding === 'hex') return outBuf.toString('hex');
                        if (outputEncoding === 'base64') return outBuf.toString('base64');
                        if (outputEncoding === 'utf8' || outputEncoding === 'utf-8') return outBuf.toString('utf8');
                        return outBuf;
                    },
                    setAutoPadding(autoPadding) { return this; }, // PKCS7 padding for CBC, no padding for CTR
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

            // Digital signatures - Ed25519 and ECDSA P-256
            createSign: function(algorithm, options) {
                const algo = algorithm.toLowerCase().replace(/-/g, '');
                // Supported: ed25519, sha256 (for ECDSA P-256), rsa-sha256 (future)
                const isEd25519 = algo === 'ed25519';
                const isEcdsaSha256 = algo === 'sha256' || algo === 'ecdsa' || algo === 'ecdsasha256';
                const isRsaSha256 = algo === 'rsasha256' || algo === 'sha256withrsa';

                if (!isEd25519 && !isEcdsaSha256 && !isRsaSha256) {
                    throw new Error('Unsupported algorithm: ' + algorithm + ' (supported: ed25519, sha256, RSA-SHA256)');
                }

                let data = Buffer.alloc(0);
                return {
                    update(input, inputEncoding) {
                        const buf = Buffer.isBuffer(input) ? input :
                            inputEncoding === 'hex' ? Buffer.from(input, 'hex') :
                            inputEncoding === 'base64' ? Buffer.from(input, 'base64') :
                            Buffer.from(input, inputEncoding || 'utf8');
                        data = Buffer.concat([data, buf]);
                        return this;
                    },
                    sign(privateKey, outputEncoding) {
                        let keyBuf;
                        let keyType = 'unknown';

                        // Extract key buffer and determine type
                        if (Buffer.isBuffer(privateKey)) {
                            keyBuf = privateKey;
                            keyType = keyBuf.length === 32 ? (isEcdsaSha256 ? 'ec' : 'ed25519') : 'ec';
                        } else if (privateKey && privateKey.key) {
                            keyBuf = Buffer.isBuffer(privateKey.key) ? privateKey.key : Buffer.from(privateKey.key);
                            keyType = privateKey.type || (keyBuf.length === 32 ? (isEcdsaSha256 ? 'ec' : 'ed25519') : 'ec');
                        } else if (privateKey instanceof Uint8Array) {
                            keyBuf = Buffer.from(privateKey);
                            keyType = keyBuf.length === 32 ? (isEcdsaSha256 ? 'ec' : 'ed25519') : 'ec';
                        } else {
                            throw new Error('Private key must be Buffer or object with key property');
                        }

                        let sig;
                        if (isEd25519 || (keyType === 'ed25519' && keyBuf.length === 32)) {
                            sig = _crypto.ed25519Sign(
                                keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.length),
                                data.buffer.slice(data.byteOffset, data.byteOffset + data.length)
                            );
                        } else if (isEcdsaSha256 || keyType === 'ec') {
                            // ECDSA P-256 signing
                            if (!_crypto.p256Sign) {
                                throw new Error('ECDSA P-256 signing not available');
                            }
                            sig = _crypto.p256Sign(
                                keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.length),
                                data.buffer.slice(data.byteOffset, data.byteOffset + data.length)
                            );
                        } else if (isRsaSha256) {
                            // RSA-SHA256 signing using native rsaSign (handles PEM/DER)
                            if (!_crypto.rsaSign) {
                                throw new Error('RSA-SHA256 signing not available');
                            }
                            sig = _crypto.rsaSign(
                                keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.length),
                                data.buffer.slice(data.byteOffset, data.byteOffset + data.length)
                            );
                        } else {
                            throw new Error('Cannot determine key type for signing');
                        }

                        const result = Buffer.from(sig);
                        if (outputEncoding === 'hex') return result.toString('hex');
                        if (outputEncoding === 'base64') return result.toString('base64');
                        return result;
                    }
                };
            },
            createVerify: function(algorithm, options) {
                const algo = algorithm.toLowerCase().replace(/-/g, '');
                const isEd25519 = algo === 'ed25519';
                const isEcdsaSha256 = algo === 'sha256' || algo === 'ecdsa' || algo === 'ecdsasha256';
                const isRsaSha256 = algo === 'rsasha256' || algo === 'sha256withrsa';

                if (!isEd25519 && !isEcdsaSha256 && !isRsaSha256) {
                    throw new Error('Unsupported algorithm: ' + algorithm + ' (supported: ed25519, sha256, RSA-SHA256)');
                }

                let data = Buffer.alloc(0);
                return {
                    update(input, inputEncoding) {
                        const buf = Buffer.isBuffer(input) ? input :
                            inputEncoding === 'hex' ? Buffer.from(input, 'hex') :
                            inputEncoding === 'base64' ? Buffer.from(input, 'base64') :
                            Buffer.from(input, inputEncoding || 'utf8');
                        data = Buffer.concat([data, buf]);
                        return this;
                    },
                    verify(publicKey, signature, signatureEncoding) {
                        let keyBuf;
                        let keyType = 'unknown';

                        // Extract key buffer and determine type
                        if (Buffer.isBuffer(publicKey)) {
                            keyBuf = publicKey;
                            // Ed25519 pubkey is 32 bytes, P-256 is 65 (uncompressed) or 33 (compressed)
                            keyType = keyBuf.length === 32 ? 'ed25519' : (keyBuf.length === 65 || keyBuf.length === 33) ? 'ec' : 'unknown';
                        } else if (publicKey && publicKey.key) {
                            keyBuf = Buffer.isBuffer(publicKey.key) ? publicKey.key : Buffer.from(publicKey.key);
                            keyType = publicKey.type || (keyBuf.length === 32 ? 'ed25519' : 'ec');
                        } else if (publicKey instanceof Uint8Array) {
                            keyBuf = Buffer.from(publicKey);
                            keyType = keyBuf.length === 32 ? 'ed25519' : 'ec';
                        } else {
                            throw new Error('Public key must be Buffer or object with key property');
                        }

                        let sigBuf = Buffer.isBuffer(signature) ? signature :
                            signatureEncoding === 'hex' ? Buffer.from(signature, 'hex') :
                            signatureEncoding === 'base64' ? Buffer.from(signature, 'base64') :
                            Buffer.from(signature);

                        if (isEd25519 || keyType === 'ed25519') {
                            return _crypto.ed25519Verify(
                                keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.length),
                                data.buffer.slice(data.byteOffset, data.byteOffset + data.length),
                                sigBuf.buffer.slice(sigBuf.byteOffset, sigBuf.byteOffset + sigBuf.length)
                            );
                        } else if (isEcdsaSha256 || keyType === 'ec') {
                            // ECDSA P-256 verification
                            if (!_crypto.p256Verify) {
                                throw new Error('ECDSA P-256 verification not available');
                            }
                            return _crypto.p256Verify(
                                keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.length),
                                data.buffer.slice(data.byteOffset, data.byteOffset + data.length),
                                sigBuf.buffer.slice(sigBuf.byteOffset, sigBuf.byteOffset + sigBuf.length)
                            );
                        } else if (isRsaSha256) {
                            // RSA-SHA256 verification using native rsaVerify (handles PEM/DER)
                            if (!_crypto.rsaVerify) {
                                throw new Error('RSA-SHA256 verification not available');
                            }
                            return _crypto.rsaVerify(
                                keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.length),
                                data.buffer.slice(data.byteOffset, data.byteOffset + data.length),
                                sigBuf.buffer.slice(sigBuf.byteOffset, sigBuf.byteOffset + sigBuf.length)
                            );
                        }

                        throw new Error('Cannot determine key type for verification');
                    }
                };
            },
            sign: function(algorithm, data, key, callback) {
                const algo = algorithm.toLowerCase().replace(/-/g, '');
                const supported = ['ed25519', 'sha256', 'ecdsa', 'ecdsasha256'];
                if (!supported.includes(algo)) throw new Error('Unsupported algorithm: ' + algorithm);
                const signer = _modules.crypto.createSign(algorithm);
                signer.update(data);
                const sig = signer.sign(key);
                if (callback) setTimeout(() => callback(null, sig), 0);
                return sig;
            },
            verify: function(algorithm, data, key, signature, callback) {
                const algo = algorithm.toLowerCase().replace(/-/g, '');
                const supported = ['ed25519', 'sha256', 'ecdsa', 'ecdsasha256'];
                if (!supported.includes(algo)) throw new Error('Unsupported algorithm: ' + algorithm);
                const verifier = _modules.crypto.createVerify(algorithm);
                verifier.update(data);
                const result = verifier.verify(key, signature);
                if (callback) setTimeout(() => callback(null, result), 0);
                return result;
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

            // Prime number generation and testing
            generatePrime: function(size, options, callback) {
                if (typeof options === 'function') { callback = options; options = {}; }
                try {
                    const result = _modules.crypto.generatePrimeSync(size, options);
                    if (callback) setTimeout(() => callback(null, result), 0);
                    return result;
                } catch (e) {
                    if (callback) setTimeout(() => callback(e), 0);
                    throw e;
                }
            },
            generatePrimeSync: function(size, options) {
                if (!_crypto?.generatePrimeSync) {
                    throw new Error('generatePrimeSync not available - native crypto not registered');
                }
                const result = _crypto.generatePrimeSync(size);
                return Buffer.from(result);
            },
            checkPrime: function(candidate, options, callback) {
                if (typeof options === 'function') { callback = options; options = {}; }
                try {
                    const result = _modules.crypto.checkPrimeSync(candidate, options);
                    if (callback) setTimeout(() => callback(null, result), 0);
                    return result;
                } catch (e) {
                    if (callback) setTimeout(() => callback(e), 0);
                    throw e;
                }
            },
            checkPrimeSync: function(candidate, options) {
                if (!_crypto?.checkPrimeSync) {
                    throw new Error('checkPrimeSync not available - native crypto not registered');
                }
                // Convert Buffer/Uint8Array to ArrayBuffer
                let candidateBuffer;
                if (Buffer.isBuffer(candidate)) {
                    candidateBuffer = candidate.buffer.slice(candidate.byteOffset, candidate.byteOffset + candidate.length);
                } else if (candidate instanceof Uint8Array) {
                    candidateBuffer = candidate.buffer.slice(candidate.byteOffset, candidate.byteOffset + candidate.length);
                } else if (candidate instanceof ArrayBuffer) {
                    candidateBuffer = candidate;
                } else if (typeof candidate === 'bigint') {
                    // Convert BigInt to bytes
                    const hex = candidate.toString(16);
                    const paddedHex = hex.length % 2 ? '0' + hex : hex;
                    const bytes = [];
                    for (let i = 0; i < paddedHex.length; i += 2) {
                        bytes.push(parseInt(paddedHex.substr(i, 2), 16));
                    }
                    candidateBuffer = new Uint8Array(bytes).buffer;
                } else {
                    throw new TypeError('candidate must be Buffer, Uint8Array, ArrayBuffer, or BigInt');
                }
                return _crypto.checkPrimeSync(candidateBuffer);
            },

            // Key generation for ed25519 and x25519
            generateKeyPair: function(type, options, callback) {
                if (typeof options === 'function') { callback = options; options = {}; }
                try {
                    const result = _modules.crypto.generateKeyPairSync(type, options);
                    if (callback) setTimeout(() => callback(null, result.publicKey, result.privateKey), 0);
                } catch (e) {
                    if (callback) setTimeout(() => callback(e), 0);
                }
            },
            generateKeyPairSync: function(type, options) {
                const algo = type.toLowerCase();
                options = options || {};

                // Ed25519 / X25519
                if (algo === 'ed25519' || algo === 'x25519') {
                    const result = _crypto.generateKeyPairSync(algo);
                    if (!result) throw new Error('generateKeyPairSync failed');
                    return {
                        publicKey: Buffer.from(result.publicKey),
                        privateKey: Buffer.from(result.privateKey)
                    };
                }

                // EC (ECDSA/ECDH)
                if (algo === 'ec') {
                    const curve = (options.namedCurve || 'prime256v1').toLowerCase();

                    // P-256 curve
                    if (curve === 'prime256v1' || curve === 'p-256' || curve === 'secp256r1') {
                        if (!_crypto.p256GenerateKeyPair) {
                            throw new Error('ECDSA P-256 key generation not available');
                        }
                        const result = _crypto.p256GenerateKeyPair();
                        if (!result) throw new Error('P-256 key generation failed');

                        const privateKeyObj = {
                            type: 'private',
                            asymmetricKeyType: 'ec',
                            asymmetricKeyDetails: { namedCurve: 'prime256v1' },
                            _raw: Buffer.from(result.privateKey),
                            _publicKey: Buffer.from(result.publicKey),
                            export: function(exportOptions) {
                                exportOptions = exportOptions || {};
                                if (exportOptions.format === 'jwk') {
                                    // EC JWK format
                                    const pubKey = this._publicKey;
                                    return {
                                        kty: 'EC',
                                        crv: 'P-256',
                                        x: pubKey.slice(1, 33).toString('base64url'),
                                        y: pubKey.slice(33, 65).toString('base64url'),
                                        d: this._raw.toString('base64url')
                                    };
                                }
                                return this._raw;
                            }
                        };

                        const publicKeyObj = {
                            type: 'public',
                            asymmetricKeyType: 'ec',
                            asymmetricKeyDetails: { namedCurve: 'prime256v1' },
                            _raw: Buffer.from(result.publicKey),
                            export: function(exportOptions) {
                                exportOptions = exportOptions || {};
                                if (exportOptions.format === 'jwk') {
                                    return {
                                        kty: 'EC',
                                        crv: 'P-256',
                                        x: this._raw.slice(1, 33).toString('base64url'),
                                        y: this._raw.slice(33, 65).toString('base64url')
                                    };
                                }
                                return this._raw;
                            }
                        };

                        return {
                            publicKey: publicKeyObj,
                            privateKey: privateKeyObj
                        };
                    }

                    // P-384 curve
                    if (curve === 'secp384r1' || curve === 'p-384') {
                        if (!_crypto.p384GenerateKeyPair) {
                            throw new Error('ECDSA P-384 key generation not available');
                        }
                        const result = _crypto.p384GenerateKeyPair();
                        if (!result) throw new Error('P-384 key generation failed');

                        const privateKeyObj = {
                            type: 'private',
                            asymmetricKeyType: 'ec',
                            asymmetricKeyDetails: { namedCurve: 'secp384r1' },
                            _raw: Buffer.from(result.privateKey),
                            _publicKey: Buffer.from(result.publicKey),
                            export: function(exportOptions) {
                                exportOptions = exportOptions || {};
                                if (exportOptions.format === 'jwk') {
                                    const pubKey = this._publicKey;
                                    return {
                                        kty: 'EC',
                                        crv: 'P-384',
                                        x: pubKey.slice(1, 49).toString('base64url'),
                                        y: pubKey.slice(49, 97).toString('base64url'),
                                        d: this._raw.toString('base64url')
                                    };
                                }
                                return this._raw;
                            }
                        };

                        const publicKeyObj = {
                            type: 'public',
                            asymmetricKeyType: 'ec',
                            asymmetricKeyDetails: { namedCurve: 'secp384r1' },
                            _raw: Buffer.from(result.publicKey),
                            export: function(exportOptions) {
                                exportOptions = exportOptions || {};
                                if (exportOptions.format === 'jwk') {
                                    return {
                                        kty: 'EC',
                                        crv: 'P-384',
                                        x: this._raw.slice(1, 49).toString('base64url'),
                                        y: this._raw.slice(49, 97).toString('base64url')
                                    };
                                }
                                return this._raw;
                            }
                        };

                        return {
                            publicKey: publicKeyObj,
                            privateKey: privateKeyObj
                        };
                    }

                    throw new Error('Unsupported curve: ' + curve + ' (supported: prime256v1, P-256, secp384r1, P-384)');
                }

                // RSA key generation
                if (algo === 'rsa' || algo === 'rsa-pss') {
                    const modulusLength = options.modulusLength || 2048;
                    if (modulusLength !== 2048 && modulusLength !== 3072 && modulusLength !== 4096) {
                        throw new Error('modulusLength must be 2048, 3072, or 4096');
                    }
                    if (!_crypto.rsaGenerateKeyPairSync) {
                        throw new Error('RSA key generation not available - native crypto not registered');
                    }
                    const result = _crypto.rsaGenerateKeyPairSync(modulusLength);
                    if (!result) throw new Error('RSA key generation failed');

                    // Create KeyObject-like structures
                    const privateKeyObj = {
                        type: 'private',
                        asymmetricKeyType: 'rsa',
                        asymmetricKeyDetails: {
                            modulusLength: modulusLength,
                            publicExponent: 65537n
                        },
                        _n: Buffer.from(result.n),
                        _e: Buffer.from(result.e),
                        _d: Buffer.from(result.d),
                        export: function(exportOptions) {
                            exportOptions = exportOptions || {};
                            if (exportOptions.format === 'jwk') {
                                return {
                                    kty: 'RSA',
                                    n: this._n.toString('base64url'),
                                    e: this._e.toString('base64url'),
                                    d: this._d.toString('base64url')
                                };
                            }
                            // Return raw DER components
                            return { n: this._n, e: this._e, d: this._d };
                        }
                    };

                    const publicKeyObj = {
                        type: 'public',
                        asymmetricKeyType: 'rsa',
                        asymmetricKeyDetails: {
                            modulusLength: modulusLength,
                            publicExponent: 65537n
                        },
                        _n: Buffer.from(result.n),
                        _e: Buffer.from(result.e),
                        export: function(exportOptions) {
                            exportOptions = exportOptions || {};
                            if (exportOptions.format === 'jwk') {
                                return {
                                    kty: 'RSA',
                                    n: this._n.toString('base64url'),
                                    e: this._e.toString('base64url')
                                };
                            }
                            return { n: this._n, e: this._e };
                        }
                    };

                    return {
                        publicKey: publicKeyObj,
                        privateKey: privateKeyObj
                    };
                }

                // DSA - not supported
                if (algo === 'dsa') {
                    throw new Error('DSA not supported - use EC or Ed25519 instead');
                }

                throw new Error('Unsupported algorithm: ' + type + ' (supported: ed25519, x25519, ec)');
            },
            generateKey: function(type, options, callback) {
                if (typeof options === 'function') { callback = options; options = {}; }
                if (callback) setTimeout(() => callback(new Error('generateKey not implemented for symmetric keys')), 0);
            },
            generateKeySync: function(type, options) {
                throw new Error('generateKeySync not implemented for symmetric keys');
            },

            // Key object implementations - Ed25519 and X25519 support
            createPrivateKey: function(key) {
                // Handle various input formats
                let keyBuffer;
                let keyType = 'ed25519'; // Default to Ed25519

                if (Buffer.isBuffer(key)) {
                    keyBuffer = key;
                } else if (key instanceof Uint8Array) {
                    keyBuffer = Buffer.from(key);
                } else if (typeof key === 'object' && key !== null) {
                    if (key.key) {
                        // { key: Buffer/string, format: 'pem'/'der'/'jwk', type: 'pkcs8'/'sec1' }
                        keyBuffer = Buffer.isBuffer(key.key) ? key.key :
                            key.encoding ? Buffer.from(key.key, key.encoding) :
                            Buffer.from(key.key);
                        if (key.format === 'jwk') {
                            // JWK format - parse the key
                            const jwk = typeof key.key === 'string' ? JSON.parse(key.key) : key.key;
                            if (jwk.crv === 'Ed25519' || jwk.kty === 'OKP') {
                                keyType = 'ed25519';
                                // d is the private key in base64url
                                if (jwk.d) {
                                    const base64 = jwk.d.replace(/-/g, '+').replace(/_/g, '/');
                                    keyBuffer = Buffer.from(base64, 'base64');
                                }
                            } else if (jwk.crv === 'X25519') {
                                keyType = 'x25519';
                                if (jwk.d) {
                                    const base64 = jwk.d.replace(/-/g, '+').replace(/_/g, '/');
                                    keyBuffer = Buffer.from(base64, 'base64');
                                }
                            }
                        }
                    } else if (key.kty) {
                        // Direct JWK object
                        if (key.crv === 'Ed25519' || (key.kty === 'OKP' && !key.crv)) {
                            keyType = 'ed25519';
                            if (key.d) {
                                const base64 = key.d.replace(/-/g, '+').replace(/_/g, '/');
                                keyBuffer = Buffer.from(base64, 'base64');
                            }
                        } else if (key.crv === 'X25519') {
                            keyType = 'x25519';
                            if (key.d) {
                                const base64 = key.d.replace(/-/g, '+').replace(/_/g, '/');
                                keyBuffer = Buffer.from(base64, 'base64');
                            }
                        }
                    }
                } else if (typeof key === 'string') {
                    // Assume hex or base64 encoded
                    keyBuffer = Buffer.from(key, 'hex');
                    if (keyBuffer.length === 0) {
                        keyBuffer = Buffer.from(key, 'base64');
                    }
                }

                if (!keyBuffer || keyBuffer.length !== 32) {
                    throw new Error('Invalid private key: must be 32 bytes for Ed25519/X25519');
                }

                return {
                    type: 'private',
                    asymmetricKeyType: keyType,
                    asymmetricKeyDetails: { namedCurve: keyType === 'ed25519' ? 'Ed25519' : 'X25519' },
                    export: function(options) {
                        options = options || {};
                        if (options.format === 'jwk') {
                            const base64url = keyBuffer.toString('base64')
                                .replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');
                            return {
                                kty: 'OKP',
                                crv: keyType === 'ed25519' ? 'Ed25519' : 'X25519',
                                d: base64url
                            };
                        }
                        return keyBuffer;
                    },
                    equals: function(other) {
                        if (!other || other.type !== 'private') return false;
                        const otherBuf = other.export();
                        return keyBuffer.equals(otherBuf);
                    }
                };
            },

            createPublicKey: function(key) {
                let keyBuffer;
                let keyType = 'ed25519';

                if (Buffer.isBuffer(key)) {
                    keyBuffer = key;
                } else if (key instanceof Uint8Array) {
                    keyBuffer = Buffer.from(key);
                } else if (typeof key === 'object' && key !== null) {
                    // Check if it's a private KeyObject - derive public key
                    if (key.type === 'private' && key.export) {
                        const privateKey = key.export();
                        keyType = key.asymmetricKeyType || 'ed25519';
                        // Derive public key from private key using native functions
                        if (keyType === 'ed25519') {
                            if (!_crypto.ed25519DerivePublicKey) {
                                throw new Error('ed25519DerivePublicKey not available');
                            }
                            const pubKeyArr = _crypto.ed25519DerivePublicKey(
                                privateKey.buffer.slice(privateKey.byteOffset, privateKey.byteOffset + privateKey.length)
                            );
                            keyBuffer = Buffer.from(pubKeyArr);
                        } else if (keyType === 'x25519') {
                            if (!_crypto.x25519DerivePublicKey) {
                                throw new Error('x25519DerivePublicKey not available');
                            }
                            const pubKeyArr = _crypto.x25519DerivePublicKey(
                                privateKey.buffer.slice(privateKey.byteOffset, privateKey.byteOffset + privateKey.length)
                            );
                            keyBuffer = Buffer.from(pubKeyArr);
                        } else {
                            throw new Error('Cannot derive public key for key type: ' + keyType);
                        }
                    } else if (key.key) {
                        keyBuffer = Buffer.isBuffer(key.key) ? key.key :
                            key.encoding ? Buffer.from(key.key, key.encoding) :
                            Buffer.from(key.key);
                        if (key.format === 'jwk') {
                            const jwk = typeof key.key === 'string' ? JSON.parse(key.key) : key.key;
                            if (jwk.crv === 'Ed25519' || jwk.kty === 'OKP') {
                                keyType = 'ed25519';
                                if (jwk.x) {
                                    const base64 = jwk.x.replace(/-/g, '+').replace(/_/g, '/');
                                    keyBuffer = Buffer.from(base64, 'base64');
                                }
                            } else if (jwk.crv === 'X25519') {
                                keyType = 'x25519';
                                if (jwk.x) {
                                    const base64 = jwk.x.replace(/-/g, '+').replace(/_/g, '/');
                                    keyBuffer = Buffer.from(base64, 'base64');
                                }
                            }
                        }
                    } else if (key.kty) {
                        // Direct JWK object
                        if (key.crv === 'Ed25519' || (key.kty === 'OKP' && !key.crv)) {
                            keyType = 'ed25519';
                            if (key.x) {
                                const base64 = key.x.replace(/-/g, '+').replace(/_/g, '/');
                                keyBuffer = Buffer.from(base64, 'base64');
                            }
                        } else if (key.crv === 'X25519') {
                            keyType = 'x25519';
                            if (key.x) {
                                const base64 = key.x.replace(/-/g, '+').replace(/_/g, '/');
                                keyBuffer = Buffer.from(base64, 'base64');
                            }
                        }
                    }
                } else if (typeof key === 'string') {
                    keyBuffer = Buffer.from(key, 'hex');
                    if (keyBuffer.length === 0) {
                        keyBuffer = Buffer.from(key, 'base64');
                    }
                }

                if (!keyBuffer || keyBuffer.length !== 32) {
                    throw new Error('Invalid public key: must be 32 bytes for Ed25519/X25519');
                }

                return {
                    type: 'public',
                    asymmetricKeyType: keyType,
                    asymmetricKeyDetails: { namedCurve: keyType === 'ed25519' ? 'Ed25519' : 'X25519' },
                    export: function(options) {
                        options = options || {};
                        if (options.format === 'jwk') {
                            const base64url = keyBuffer.toString('base64')
                                .replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');
                            return {
                                kty: 'OKP',
                                crv: keyType === 'ed25519' ? 'Ed25519' : 'X25519',
                                x: base64url
                            };
                        }
                        return keyBuffer;
                    },
                    equals: function(other) {
                        if (!other || other.type !== 'public') return false;
                        const otherBuf = other.export();
                        return keyBuffer.equals(otherBuf);
                    }
                };
            },

            // DiffieHellman implementation with MODP groups (RFC 3526)
            // MODP group primes and generators
            _modpGroups: {
                // modp1 (768-bit) - RFC 2409
                modp1: {
                    prime: 'FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A63A3620FFFFFFFFFFFFFFFF',
                    generator: '02'
                },
                // modp2 (1024-bit) - RFC 2409
                modp2: {
                    prime: 'FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE65381FFFFFFFFFFFFFFFF',
                    generator: '02'
                },
                // modp5 (1536-bit) - RFC 3526
                modp5: {
                    prime: 'FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3DC2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F83655D23DCA3AD961C62F356208552BB9ED529077096966D670C354E4ABC9804F1746C08CA237327FFFFFFFFFFFFFFFF',
                    generator: '02'
                },
                // modp14 (2048-bit) - RFC 3526
                modp14: {
                    prime: 'FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3DC2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F83655D23DCA3AD961C62F356208552BB9ED529077096966D670C354E4ABC9804F1746C08CA18217C32905E462E36CE3BE39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9DE2BCBF6955817183995497CEA956AE515D2261898FA051015728E5A8AACAA68FFFFFFFFFFFFFFFF',
                    generator: '02'
                },
                // modp15 (3072-bit) - RFC 3526
                modp15: {
                    prime: 'FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3DC2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F83655D23DCA3AD961C62F356208552BB9ED529077096966D670C354E4ABC9804F1746C08CA18217C32905E462E36CE3BE39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9DE2BCBF6955817183995497CEA956AE515D2261898FA051015728E5A8AAAC42DAD33170D04507A33A85521ABDF1CBA64ECFB850458DBEF0A8AEA71575D060C7DB3970F85A6E1E4C7ABF5AE8CDB0933D71E8C94E04A25619DCEE3D2261AD2EE6BF12FFA06D98A0864D87602733EC86A64521F2B18177B200CBBE117577A615D6C770988C0BAD946E208E24FA074E5AB3143DB5BFCE0FD108E4B82D120A93AD2CAFFFFFFFFFFFFFFFF',
                    generator: '02'
                },
                // modp16 (4096-bit) - RFC 3526
                modp16: {
                    prime: 'FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3DC2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F83655D23DCA3AD961C62F356208552BB9ED529077096966D670C354E4ABC9804F1746C08CA18217C32905E462E36CE3BE39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9DE2BCBF6955817183995497CEA956AE515D2261898FA051015728E5A8AAAC42DAD33170D04507A33A85521ABDF1CBA64ECFB850458DBEF0A8AEA71575D060C7DB3970F85A6E1E4C7ABF5AE8CDB0933D71E8C94E04A25619DCEE3D2261AD2EE6BF12FFA06D98A0864D87602733EC86A64521F2B18177B200CBBE117577A615D6C770988C0BAD946E208E24FA074E5AB3143DB5BFCE0FD108E4B82D120A92108011A723C12A787E6D788719A10BDBA5B2699C327186AF4E23C1A946834B6150BDA2583E9CA2AD44CE8DBBBC2DB04DE8EF92E8EFC141FBECAA6287C59474E6BC05D99B2964FA090C3A2233BA186515BE7ED1F612970CEE2D7AFB81BDD762170481CD0069127D5B05AA993B4EA988D8FDDC186FFB7DC90A6C08F4DF435C934063199FFFFFFFFFFFFFFFF',
                    generator: '02'
                }
            },

            // Helper: modular exponentiation using BigInt
            _modPow: function(base, exp, mod) {
                let result = 1n;
                base = base % mod;
                while (exp > 0n) {
                    if (exp % 2n === 1n) {
                        result = (result * base) % mod;
                    }
                    exp = exp >> 1n;
                    base = (base * base) % mod;
                }
                return result;
            },

            // Helper: convert BigInt to Buffer (big-endian)
            _bigIntToBuffer: function(bigint) {
                let hex = bigint.toString(16);
                if (hex.length % 2 !== 0) hex = '0' + hex;
                return Buffer.from(hex, 'hex');
            },

            // Helper: convert Buffer to BigInt
            _bufferToBigInt: function(buf) {
                return BigInt('0x' + buf.toString('hex'));
            },

            createDiffieHellman: function(primeOrLength, primeEncodingOrGenerator, generator, generatorEncoding) {
                const self = _modules.crypto;
                let primeBuf, generatorBuf;

                // Handle different argument patterns:
                // createDiffieHellman(primeLength) - generate prime
                // createDiffieHellman(prime, [primeEncoding], [generator], [generatorEncoding])
                if (typeof primeOrLength === 'number') {
                    // For security, we don't generate random primes - use predefined groups instead
                    throw new Error('Random prime generation not supported - use getDiffieHellman() with predefined groups');
                }

                // Prime provided
                if (Buffer.isBuffer(primeOrLength)) {
                    primeBuf = primeOrLength;
                } else if (typeof primeOrLength === 'string') {
                    const encoding = typeof primeEncodingOrGenerator === 'string' && !generator ? primeEncodingOrGenerator : 'hex';
                    primeBuf = Buffer.from(primeOrLength, encoding);
                } else {
                    primeBuf = Buffer.from(primeOrLength);
                }

                // Generator (default 2)
                if (generator !== undefined) {
                    if (Buffer.isBuffer(generator)) {
                        generatorBuf = generator;
                    } else if (typeof generator === 'number') {
                        generatorBuf = Buffer.from([generator]);
                    } else {
                        generatorBuf = Buffer.from(generator, generatorEncoding || 'hex');
                    }
                } else if (typeof primeEncodingOrGenerator === 'number') {
                    generatorBuf = Buffer.from([primeEncodingOrGenerator]);
                } else if (Buffer.isBuffer(primeEncodingOrGenerator)) {
                    generatorBuf = primeEncodingOrGenerator;
                } else {
                    generatorBuf = Buffer.from([2]); // Default generator
                }

                const prime = self._bufferToBigInt(primeBuf);
                const gen = self._bufferToBigInt(generatorBuf);
                let privateKey = null;
                let publicKey = null;

                return {
                    generateKeys(encoding, format) {
                        // Generate random private key (same bit length as prime minus 1)
                        const primeBytes = primeBuf.length;
                        const randBytes = _modules.crypto.randomBytes(primeBytes);
                        let priv = self._bufferToBigInt(randBytes);
                        // Ensure private key is less than prime - 1
                        priv = priv % (prime - 2n) + 1n;
                        privateKey = self._bigIntToBuffer(priv);
                        // Compute public key: g^priv mod p
                        const pub = self._modPow(gen, priv, prime);
                        publicKey = self._bigIntToBuffer(pub);
                        // Pad to prime length
                        if (publicKey.length < primeBytes) {
                            const padded = Buffer.alloc(primeBytes);
                            publicKey.copy(padded, primeBytes - publicKey.length);
                            publicKey = padded;
                        }
                        if (encoding === 'hex') return publicKey.toString('hex');
                        if (encoding === 'base64') return publicKey.toString('base64');
                        return publicKey;
                    },
                    computeSecret(otherPublicKey, inputEncoding, outputEncoding) {
                        if (!privateKey) throw new Error('Keys not generated');
                        let otherPubBuf = Buffer.isBuffer(otherPublicKey) ? otherPublicKey :
                            inputEncoding === 'hex' ? Buffer.from(otherPublicKey, 'hex') :
                            inputEncoding === 'base64' ? Buffer.from(otherPublicKey, 'base64') :
                            Buffer.from(otherPublicKey);
                        const otherPub = self._bufferToBigInt(otherPubBuf);
                        const priv = self._bufferToBigInt(privateKey);
                        // Compute shared secret: otherPub^priv mod p
                        const secret = self._modPow(otherPub, priv, prime);
                        let secretBuf = self._bigIntToBuffer(secret);
                        // Pad to prime length
                        if (secretBuf.length < primeBuf.length) {
                            const padded = Buffer.alloc(primeBuf.length);
                            secretBuf.copy(padded, primeBuf.length - secretBuf.length);
                            secretBuf = padded;
                        }
                        if (outputEncoding === 'hex') return secretBuf.toString('hex');
                        if (outputEncoding === 'base64') return secretBuf.toString('base64');
                        return secretBuf;
                    },
                    getPrime(encoding) {
                        if (encoding === 'hex') return primeBuf.toString('hex');
                        if (encoding === 'base64') return primeBuf.toString('base64');
                        return primeBuf;
                    },
                    getGenerator(encoding) {
                        if (encoding === 'hex') return generatorBuf.toString('hex');
                        if (encoding === 'base64') return generatorBuf.toString('base64');
                        return generatorBuf;
                    },
                    getPrivateKey(encoding) {
                        if (!privateKey) throw new Error('Keys not generated');
                        if (encoding === 'hex') return privateKey.toString('hex');
                        if (encoding === 'base64') return privateKey.toString('base64');
                        return privateKey;
                    },
                    getPublicKey(encoding) {
                        if (!publicKey) throw new Error('Keys not generated');
                        if (encoding === 'hex') return publicKey.toString('hex');
                        if (encoding === 'base64') return publicKey.toString('base64');
                        return publicKey;
                    },
                    setPrivateKey(key, encoding) {
                        privateKey = Buffer.isBuffer(key) ? key :
                            encoding === 'hex' ? Buffer.from(key, 'hex') :
                            encoding === 'base64' ? Buffer.from(key, 'base64') :
                            Buffer.from(key);
                    },
                    setPublicKey(key, encoding) {
                        publicKey = Buffer.isBuffer(key) ? key :
                            encoding === 'hex' ? Buffer.from(key, 'hex') :
                            encoding === 'base64' ? Buffer.from(key, 'base64') :
                            Buffer.from(key);
                    }
                };
            },
            createDiffieHellmanGroup: function(name) {
                return _modules.crypto.getDiffieHellman(name);
            },
            // ECDH key exchange (X25519, P-256, P-384)
            createECDH: function(curveName) {
                const curve = curveName.toLowerCase();
                const isX25519 = curve === 'x25519';
                const isP256 = curve === 'prime256v1' || curve === 'p-256' || curve === 'secp256r1';
                const isP384 = curve === 'secp384r1' || curve === 'p-384';

                if (!isX25519 && !isP256 && !isP384) {
                    throw new Error('Unsupported curve: ' + curveName + ' (supported: x25519, P-256, P-384)');
                }

                let privateKey = null;
                let publicKey = null;

                return {
                    generateKeys(encoding, format) {
                        if (isX25519) {
                            const keypair = _crypto.x25519GenerateKeyPair();
                            privateKey = Buffer.from(keypair.privateKey);
                            publicKey = Buffer.from(keypair.publicKey);
                        } else if (isP256) {
                            if (!_crypto.p256GenerateKeyPair) {
                                throw new Error('P-256 key generation not available');
                            }
                            const keypair = _crypto.p256GenerateKeyPair();
                            privateKey = Buffer.from(keypair.privateKey);
                            publicKey = Buffer.from(keypair.publicKey);
                        } else if (isP384) {
                            if (!_crypto.p384GenerateKeyPair) {
                                throw new Error('P-384 key generation not available');
                            }
                            const keypair = _crypto.p384GenerateKeyPair();
                            privateKey = Buffer.from(keypair.privateKey);
                            publicKey = Buffer.from(keypair.publicKey);
                        }
                        if (encoding === 'hex') return publicKey.toString('hex');
                        if (encoding === 'base64') return publicKey.toString('base64');
                        return publicKey;
                    },
                    computeSecret(otherPublicKey, inputEncoding, outputEncoding) {
                        if (!privateKey) throw new Error('Keys not generated');
                        let otherPubBuf = Buffer.isBuffer(otherPublicKey) ? otherPublicKey :
                            inputEncoding === 'hex' ? Buffer.from(otherPublicKey, 'hex') :
                            inputEncoding === 'base64' ? Buffer.from(otherPublicKey, 'base64') :
                            Buffer.from(otherPublicKey);

                        let secret;
                        if (isX25519) {
                            secret = _crypto.x25519ComputeSecret(
                                privateKey.buffer.slice(privateKey.byteOffset, privateKey.byteOffset + privateKey.length),
                                otherPubBuf.buffer.slice(otherPubBuf.byteOffset, otherPubBuf.byteOffset + otherPubBuf.length)
                            );
                        } else if (isP256) {
                            if (!_crypto.p256ComputeSecret) {
                                throw new Error('P-256 ECDH not available');
                            }
                            secret = _crypto.p256ComputeSecret(
                                privateKey.buffer.slice(privateKey.byteOffset, privateKey.byteOffset + privateKey.length),
                                otherPubBuf.buffer.slice(otherPubBuf.byteOffset, otherPubBuf.byteOffset + otherPubBuf.length)
                            );
                        } else if (isP384) {
                            if (!_crypto.p384ComputeSecret) {
                                throw new Error('P-384 ECDH not available');
                            }
                            secret = _crypto.p384ComputeSecret(
                                privateKey.buffer.slice(privateKey.byteOffset, privateKey.byteOffset + privateKey.length),
                                otherPubBuf.buffer.slice(otherPubBuf.byteOffset, otherPubBuf.byteOffset + otherPubBuf.length)
                            );
                        }
                        const result = Buffer.from(secret);
                        if (outputEncoding === 'hex') return result.toString('hex');
                        if (outputEncoding === 'base64') return result.toString('base64');
                        return result;
                    },
                    getPrivateKey(encoding) {
                        if (!privateKey) throw new Error('Keys not generated');
                        if (encoding === 'hex') return privateKey.toString('hex');
                        if (encoding === 'base64') return privateKey.toString('base64');
                        return privateKey;
                    },
                    getPublicKey(encoding, format) {
                        if (!publicKey) throw new Error('Keys not generated');
                        if (encoding === 'hex') return publicKey.toString('hex');
                        if (encoding === 'base64') return publicKey.toString('base64');
                        return publicKey;
                    },
                    setPrivateKey(key, encoding) {
                        privateKey = Buffer.isBuffer(key) ? key :
                            encoding === 'hex' ? Buffer.from(key, 'hex') :
                            encoding === 'base64' ? Buffer.from(key, 'base64') :
                            Buffer.from(key);
                        // Note: For setPrivateKey, call generateKeys() afterwards to derive public key
                    },
                    setPublicKey(key, encoding) {
                        publicKey = Buffer.isBuffer(key) ? key :
                            encoding === 'hex' ? Buffer.from(key, 'hex') :
                            encoding === 'base64' ? Buffer.from(key, 'base64') :
                            Buffer.from(key);
                    }
                };
            },
            getDiffieHellman: function(groupName) {
                const name = groupName.toLowerCase();
                const group = _modules.crypto._modpGroups[name];
                if (!group) {
                    throw new Error('Unknown DH group: ' + groupName + ' (supported: modp1, modp2, modp5, modp14, modp15, modp16)');
                }
                return _modules.crypto.createDiffieHellman(
                    Buffer.from(group.prime, 'hex'),
                    Buffer.from(group.generator, 'hex')
                );
            },

            // Round 14: KeyObject APIs for symmetric keys
            createSecretKey: function(key, encoding) {
                // Convert key to Buffer if string
                const keyBuffer = typeof key === 'string' ? Buffer.from(key, encoding || 'utf8') :
                    (Buffer.isBuffer(key) ? key : Buffer.from(key));

                // Return KeyObject-like object
                return {
                    type: 'secret',
                    symmetricKeySize: keyBuffer.length,
                    export: function(options) {
                        if (!options || options.format === 'buffer') return keyBuffer;
                        if (options.format === 'jwk') {
                            // Base64url encode for JWK
                            const base64 = keyBuffer.toString('base64');
                            const base64url = base64.replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');
                            return { kty: 'oct', k: base64url };
                        }
                        throw new Error('Unsupported format: ' + (options.format || 'unknown'));
                    },
                    equals: function(otherKey) {
                        if (!otherKey || otherKey.type !== 'secret') return false;
                        const otherBuffer = otherKey.export();
                        return keyBuffer.equals(otherBuffer);
                    }
                };
            },

            generateKeySync: function(type, options) {
                if (type !== 'aes' && type !== 'hmac') {
                    throw new Error('Unsupported key type: ' + type + '. Only aes and hmac are supported.');
                }
                const length = (options && options.length) || (type === 'aes' ? 256 : 512);
                if (length % 8 !== 0) {
                    throw new Error('Key length must be a multiple of 8');
                }
                const keyBytes = _modules.crypto.randomBytes(length / 8);
                return _modules.crypto.createSecretKey(keyBytes);
            },

            generateKey: function(type, options, callback) {
                // Async version - just wraps sync version
                try {
                    const key = _modules.crypto.generateKeySync(type, options);
                    if (typeof callback === 'function') {
                        setImmediate(() => callback(null, key));
                    }
                } catch (e) {
                    if (typeof callback === 'function') {
                        setImmediate(() => callback(e));
                    } else {
                        throw e;
                    }
                }
            },

            // RSA padding constants
            constants: {
                RSA_PKCS1_PADDING: 1,
                RSA_NO_PADDING: 3,
                RSA_PKCS1_OAEP_PADDING: 4,
                RSA_PSS_PADDING: 6,
                RSA_PKCS1_PSS_PADDING: 6  // Alias
            },

            // RSA public key encryption / private key decryption (Zig handles PEM/DER)
            // Supports RSA_PKCS1_PADDING (1) and RSA_PKCS1_OAEP_PADDING (4)
            publicEncrypt: function(key, buffer) {
                if (!_crypto.rsaEncrypt) {
                    throw new Error('publicEncrypt not available - Zig native not registered');
                }
                let keyBuf, padding;
                if (Buffer.isBuffer(key)) {
                    keyBuf = key;
                    padding = _modules.crypto.constants.RSA_PKCS1_OAEP_PADDING; // Default to OAEP
                } else if (typeof key === 'string') {
                    keyBuf = Buffer.from(key);
                    padding = _modules.crypto.constants.RSA_PKCS1_OAEP_PADDING;
                } else if (key && typeof key === 'object') {
                    keyBuf = Buffer.isBuffer(key.key) ? key.key :
                             Buffer.isBuffer(key.buffer) ? key.buffer :
                             key.key ? Buffer.from(key.key) :
                             key.buffer ? Buffer.from(key.buffer) : null;
                    if (!keyBuf) throw new Error('Invalid key format');
                    padding = key.padding !== undefined ? key.padding : _modules.crypto.constants.RSA_PKCS1_OAEP_PADDING;
                } else {
                    throw new Error('Invalid key format');
                }

                // Supported padding modes: PKCS#1 v1.5 (1) and OAEP (4)
                if (padding !== _modules.crypto.constants.RSA_PKCS1_PADDING &&
                    padding !== _modules.crypto.constants.RSA_PKCS1_OAEP_PADDING) {
                    throw new Error('Unsupported padding mode - use RSA_PKCS1_PADDING (1) or RSA_PKCS1_OAEP_PADDING (4)');
                }

                const dataBuf = Buffer.isBuffer(buffer) ? buffer : Buffer.from(buffer);
                const result = _crypto.rsaEncrypt(
                    keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.length),
                    dataBuf.buffer.slice(dataBuf.byteOffset, dataBuf.byteOffset + dataBuf.length),
                    padding
                );
                return Buffer.from(result);
            },

            privateDecrypt: function(key, buffer) {
                if (!_crypto.rsaDecrypt) {
                    throw new Error('privateDecrypt not available - Zig native not registered');
                }
                let keyBuf, padding;
                if (Buffer.isBuffer(key)) {
                    keyBuf = key;
                    padding = _modules.crypto.constants.RSA_PKCS1_OAEP_PADDING;
                } else if (typeof key === 'string') {
                    keyBuf = Buffer.from(key);
                    padding = _modules.crypto.constants.RSA_PKCS1_OAEP_PADDING;
                } else if (key && typeof key === 'object') {
                    keyBuf = Buffer.isBuffer(key.key) ? key.key :
                             Buffer.isBuffer(key.buffer) ? key.buffer :
                             key.key ? Buffer.from(key.key) :
                             key.buffer ? Buffer.from(key.buffer) : null;
                    if (!keyBuf) throw new Error('Invalid key format');
                    padding = key.padding !== undefined ? key.padding : _modules.crypto.constants.RSA_PKCS1_OAEP_PADDING;
                } else {
                    throw new Error('Invalid key format');
                }

                // Supported padding modes: PKCS#1 v1.5 (1) and OAEP (4)
                if (padding !== _modules.crypto.constants.RSA_PKCS1_PADDING &&
                    padding !== _modules.crypto.constants.RSA_PKCS1_OAEP_PADDING) {
                    throw new Error('Unsupported padding mode - use RSA_PKCS1_PADDING (1) or RSA_PKCS1_OAEP_PADDING (4)');
                }

                const dataBuf = Buffer.isBuffer(buffer) ? buffer : Buffer.from(buffer);
                const result = _crypto.rsaDecrypt(
                    keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.length),
                    dataBuf.buffer.slice(dataBuf.byteOffset, dataBuf.byteOffset + dataBuf.length),
                    padding
                );
                return Buffer.from(result);
            },

            // privateEncrypt - encrypt with private key (PKCS#1 v1.5 type 1 padding)
            privateEncrypt: function(key, buffer) {
                if (!_crypto.rsaPrivateEncrypt) {
                    throw new Error('privateEncrypt not available - native crypto not registered');
                }
                let keyBuf, padding;
                if (Buffer.isBuffer(key)) {
                    keyBuf = key;
                    padding = _modules.crypto.constants.RSA_PKCS1_PADDING;
                } else if (typeof key === 'string') {
                    keyBuf = Buffer.from(key);
                    padding = _modules.crypto.constants.RSA_PKCS1_PADDING;
                } else if (key && typeof key === 'object') {
                    keyBuf = Buffer.isBuffer(key.key) ? key.key :
                             Buffer.isBuffer(key.buffer) ? key.buffer :
                             key.key ? Buffer.from(key.key) :
                             key.buffer ? Buffer.from(key.buffer) : null;
                    if (!keyBuf) throw new Error('Invalid key format');
                    padding = key.padding !== undefined ? key.padding : _modules.crypto.constants.RSA_PKCS1_PADDING;
                } else {
                    throw new Error('Invalid key format');
                }

                const dataBuf = Buffer.isBuffer(buffer) ? buffer : Buffer.from(buffer);
                const result = _crypto.rsaPrivateEncrypt(
                    keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.length),
                    dataBuf.buffer.slice(dataBuf.byteOffset, dataBuf.byteOffset + dataBuf.length),
                    padding
                );
                return Buffer.from(result);
            },

            // publicDecrypt - decrypt with public key (PKCS#1 v1.5 type 1 unpadding)
            publicDecrypt: function(key, buffer) {
                if (!_crypto.rsaPublicDecrypt) {
                    throw new Error('publicDecrypt not available - native crypto not registered');
                }
                let keyBuf, padding;
                if (Buffer.isBuffer(key)) {
                    keyBuf = key;
                    padding = _modules.crypto.constants.RSA_PKCS1_PADDING;
                } else if (typeof key === 'string') {
                    keyBuf = Buffer.from(key);
                    padding = _modules.crypto.constants.RSA_PKCS1_PADDING;
                } else if (key && typeof key === 'object') {
                    keyBuf = Buffer.isBuffer(key.key) ? key.key :
                             Buffer.isBuffer(key.buffer) ? key.buffer :
                             key.key ? Buffer.from(key.key) :
                             key.buffer ? Buffer.from(key.buffer) : null;
                    if (!keyBuf) throw new Error('Invalid key format');
                    padding = key.padding !== undefined ? key.padding : _modules.crypto.constants.RSA_PKCS1_PADDING;
                } else {
                    throw new Error('Invalid key format');
                }

                const dataBuf = Buffer.isBuffer(buffer) ? buffer : Buffer.from(buffer);
                const result = _crypto.rsaPublicDecrypt(
                    keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.length),
                    dataBuf.buffer.slice(dataBuf.byteOffset, dataBuf.byteOffset + dataBuf.length),
                    padding
                );
                return Buffer.from(result);
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

