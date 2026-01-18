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
            getCiphers: () => ['aes-256-gcm'],

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
            }
        };
    }
    _modules['node:crypto'] = _modules.crypto;

