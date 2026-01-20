    // ===== CRYPTO MODULE =====
    // Zig native: src/polyfills/crypto.zig (hash, hmac, aesGcmEncrypt/Decrypt, randomBytes, randomUUID)
    // JS: thin wrapper with createHash/createHmac object interfaces
    // Note: PEM to DER conversion is handled in Zig - JS just passes key data as-is
    {
        // Fallback Transform class if stream module not loaded
        // This allows crypto to work standalone when tree-shaken
        const Transform = (typeof globalThis.Transform !== 'undefined') ? globalThis.Transform :
            (_modules.stream?.Transform) ||
            (class Transform {
                constructor(options) { this._options = options; }
                _transform(chunk, encoding, callback) { callback(); }
                _flush(callback) { callback(); }
                push(chunk) { return true; }
                pipe(dest) { return dest; }
                on(event, fn) { return this; }
                once(event, fn) { return this; }
                emit(event, ...args) { return true; }
            });

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
            // randomInt - generate cryptographically strong random integer
            // Signatures: randomInt(max) or randomInt(min, max) with optional callback
            randomInt: function(minOrMax, maxOrCallback, callback) {
                let min, max;

                // Parse arguments
                if (typeof maxOrCallback === 'function') {
                    callback = maxOrCallback;
                    max = minOrMax;
                    min = 0;
                } else if (typeof maxOrCallback === 'number') {
                    min = minOrMax;
                    max = maxOrCallback;
                } else {
                    max = minOrMax;
                    min = 0;
                }

                // Validate range
                if (!Number.isSafeInteger(min)) {
                    throw new RangeError('min must be a safe integer');
                }
                if (!Number.isSafeInteger(max)) {
                    throw new RangeError('max must be a safe integer');
                }
                if (min >= max) {
                    throw new RangeError('min must be less than max');
                }

                const range = max - min;
                if (range > 281474976710655) { // 2^48 - 1
                    throw new RangeError('Range must be less than 2^48');
                }

                // Calculate bytes needed for range
                const bytesNeeded = Math.ceil(Math.log2(range + 1) / 8) || 1;

                // Use rejection sampling to avoid modulo bias
                // Max valid value is the largest multiple of range that fits in bytesNeeded
                const maxBytes = Math.pow(256, bytesNeeded);
                const maxValid = maxBytes - (maxBytes % range);

                const generateValue = () => {
                    let result;
                    do {
                        const bytes = randomBytes(bytesNeeded);
                        result = 0;
                        for (let i = 0; i < bytesNeeded; i++) {
                            result = result * 256 + bytes[i];
                        }
                    } while (result >= maxValid);

                    return min + (result % range);
                };

                if (callback) {
                    try {
                        const value = generateValue();
                        setImmediate(() => callback(null, value));
                    } catch (err) {
                        setImmediate(() => callback(err));
                    }
                    return;
                }

                return generateValue();
            },
            // randomFill - fill buffer with random bytes at offset/size
            randomFill: function(buffer, offset, size, callback) {
                // Handle optional arguments
                if (typeof offset === 'function') {
                    callback = offset;
                    offset = 0;
                    size = buffer.length;
                } else if (typeof size === 'function') {
                    callback = size;
                    size = buffer.length - offset;
                }
                offset = offset || 0;
                size = size !== undefined ? size : buffer.length - offset;

                try {
                    _modules.crypto.randomFillSync(buffer, offset, size);
                    if (callback) setTimeout(() => callback(null, buffer), 0);
                } catch (e) {
                    if (callback) setTimeout(() => callback(e), 0);
                    else throw e;
                }
                return buffer;
            },
            // randomFillSync - synchronously fill buffer with random bytes
            randomFillSync: function(buffer, offset, size) {
                offset = offset || 0;
                size = size !== undefined ? size : buffer.length - offset;

                if (offset < 0 || size < 0 || offset + size > buffer.length) {
                    throw new RangeError('offset and size must be valid within buffer bounds');
                }

                if (size === 0) return buffer;

                const bytes = randomBytes ? randomBytes(size) : null;
                if (!bytes) throw new Error('crypto.randomBytes not available');

                // Copy bytes into the buffer at offset
                if (Buffer.isBuffer(buffer)) {
                    bytes.copy(buffer, offset);
                } else if (buffer instanceof Uint8Array) {
                    buffer.set(bytes, offset);
                } else {
                    for (let i = 0; i < size; i++) {
                        buffer[offset + i] = bytes[i];
                    }
                }

                return buffer;
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
                'aes-256-ctr', 'aes-128-ctr',
                'chacha20-poly1305'
            ],
            getCurves: () => [
                'prime256v1', 'P-256', 'secp256r1',  // P-256 ECDSA/ECDH
                'secp384r1', 'P-384',               // P-384 ECDSA/ECDH
                'x25519',                            // X25519 ECDH
                'ed25519',                           // Ed25519 signatures
                'secp256k1'                          // Bitcoin/Ethereum
            ],
            getCipherInfo: function(nameOrNid, options) {
                const ciphers = {
                    'aes-256-gcm': { name: 'aes-256-gcm', nid: 901, blockSize: 1, ivLength: 12, keyLength: 32, mode: 'gcm' },
                    'aes-128-gcm': { name: 'aes-128-gcm', nid: 895, blockSize: 1, ivLength: 12, keyLength: 16, mode: 'gcm' },
                    'aes-256-cbc': { name: 'aes-256-cbc', nid: 427, blockSize: 16, ivLength: 16, keyLength: 32, mode: 'cbc' },
                    'aes-128-cbc': { name: 'aes-128-cbc', nid: 419, blockSize: 16, ivLength: 16, keyLength: 16, mode: 'cbc' },
                    'aes-256-ctr': { name: 'aes-256-ctr', nid: 906, blockSize: 1, ivLength: 16, keyLength: 32, mode: 'ctr' },
                    'aes-128-ctr': { name: 'aes-128-ctr', nid: 904, blockSize: 1, ivLength: 16, keyLength: 16, mode: 'ctr' },
                    'chacha20-poly1305': { name: 'chacha20-poly1305', nid: 1018, blockSize: 1, ivLength: 12, keyLength: 32, mode: 'poly1305' }
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

            // Hash class - Transform stream that computes cryptographic hash
            // Node.js: createHash returns a Hash object that extends Transform
            Hash: class Hash extends Transform {
                constructor(algorithm, options) {
                    super(options);
                    this._algorithm = algorithm.toLowerCase();
                    this._data = '';
                    this._finalized = false;
                }

                _transform(chunk, encoding, callback) {
                    // Hash streams don't output data during transform - only on digest
                    this.update(chunk, encoding);
                    callback();
                }

                _flush(callback) {
                    // On stream end, emit the digest
                    try {
                        const result = this.digest();
                        this.push(result);
                        callback();
                    } catch (e) {
                        callback(e);
                    }
                }

                update(input, inputEncoding) {
                    if (this._finalized) throw new Error('Digest already called');
                    if (typeof input === 'string') {
                        if (inputEncoding === 'hex') {
                            for (let i = 0; i < input.length; i += 2) {
                                this._data += String.fromCharCode(parseInt(input.slice(i, i + 2), 16));
                            }
                        } else if (inputEncoding === 'base64') {
                            this._data += atob(input);
                        } else {
                            this._data += input;
                        }
                    } else if (Buffer.isBuffer(input) || input instanceof Uint8Array) {
                        this._data += String.fromCharCode.apply(null, input);
                    } else {
                        this._data += String(input);
                    }
                    return this;
                }

                digest(encoding) {
                    if (this._finalized) throw new Error('Digest already called');
                    this._finalized = true;

                    const result = _crypto.hash?.(this._algorithm, this._data) ?? globalThis.__edgebox_hash?.(this._algorithm, this._data);
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
                }

                copy() {
                    const newHash = new _modules.crypto.Hash(this._algorithm);
                    newHash._data = this._data;
                    return newHash;
                }
            },

            // createHash - factory for Hash Transform stream
            // Node.js signature: createHash(algorithm[, options])
            createHash: function(algorithm, options) {
                return new _modules.crypto.Hash(algorithm, options);
            },

            // Hmac class - Transform stream that computes HMAC
            // Node.js: createHmac returns an Hmac object that extends Transform
            Hmac: class Hmac extends Transform {
                constructor(algorithm, key, options) {
                    super(options);
                    this._algorithm = algorithm.toLowerCase();
                    this._key = typeof key === 'string' ? key : String.fromCharCode.apply(null, key);
                    this._data = '';
                    this._finalized = false;
                }

                _transform(chunk, encoding, callback) {
                    this.update(chunk, encoding);
                    callback();
                }

                _flush(callback) {
                    try {
                        const result = this.digest();
                        this.push(result);
                        callback();
                    } catch (e) {
                        callback(e);
                    }
                }

                update(input, inputEncoding) {
                    if (this._finalized) throw new Error('Digest already called');
                    if (typeof input === 'string') {
                        if (inputEncoding === 'hex') {
                            for (let i = 0; i < input.length; i += 2) {
                                this._data += String.fromCharCode(parseInt(input.slice(i, i + 2), 16));
                            }
                        } else if (inputEncoding === 'base64') {
                            this._data += atob(input);
                        } else {
                            this._data += input;
                        }
                    } else if (Buffer.isBuffer(input) || input instanceof Uint8Array) {
                        this._data += String.fromCharCode.apply(null, input);
                    } else {
                        this._data += String(input);
                    }
                    return this;
                }

                digest(encoding) {
                    if (this._finalized) throw new Error('Digest already called');
                    this._finalized = true;

                    const result = _crypto.hmac?.(this._algorithm, this._key, this._data) ?? globalThis.__edgebox_hmac?.(this._algorithm, this._key, this._data);
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
            },

            // createHmac - factory for Hmac Transform stream
            // Node.js signature: createHmac(algorithm, key[, options])
            createHmac: function(algorithm, key, options) {
                return new _modules.crypto.Hmac(algorithm, key, options);
            },

            // Cipher class - Transform stream for encryption
            // Node.js: createCipheriv returns a Cipher object that extends Transform
            Cipher: class Cipher extends Transform {
                constructor(algorithm, key, iv, options) {
                    super(options);
                    this._algorithm = algorithm.toLowerCase();
                    this._keyBuf = Buffer.isBuffer(key) ? key : Buffer.from(key);
                    this._ivBuf = Buffer.isBuffer(iv) ? iv : Buffer.from(iv);
                    this._buffer = Buffer.alloc(0);
                    this._finalized = false;
                    this._aadData = Buffer.alloc(0);
                    this._authTag = null;

                    // Determine mode
                    this._isCtr = this._algorithm.includes('ctr');
                    this._isGcm = this._algorithm.includes('gcm');
                    this._isChaCha = this._algorithm === 'chacha20-poly1305';

                    // Validate algorithm and key sizes
                    this._validateParams();
                }

                _validateParams() {
                    const algo = this._algorithm;
                    const keyLen = this._keyBuf.length;
                    const ivLen = this._ivBuf.length;

                    if (algo === 'aes-256-cbc' || algo === 'aes-256-ctr') {
                        if (keyLen !== 32) throw new Error(`Invalid key length for ${algo} (need 32 bytes)`);
                        if (ivLen !== 16) throw new Error('Invalid IV length (need 16 bytes)');
                    } else if (algo === 'aes-128-cbc' || algo === 'aes-128-ctr') {
                        if (keyLen !== 16) throw new Error(`Invalid key length for ${algo} (need 16 bytes)`);
                        if (ivLen !== 16) throw new Error('Invalid IV length (need 16 bytes)');
                    } else if (algo === 'aes-256-gcm') {
                        if (keyLen !== 32) throw new Error('Invalid key length for aes-256-gcm (need 32 bytes)');
                        if (ivLen !== 12) throw new Error('Invalid IV length for GCM (need 12 bytes)');
                    } else if (algo === 'aes-128-gcm') {
                        if (keyLen !== 16) throw new Error('Invalid key length for aes-128-gcm (need 16 bytes)');
                        if (ivLen !== 12) throw new Error('Invalid IV length for GCM (need 12 bytes)');
                    } else if (algo === 'chacha20-poly1305') {
                        if (keyLen !== 32) throw new Error('Invalid key length for chacha20-poly1305 (need 32 bytes)');
                        if (ivLen !== 12) throw new Error('Invalid IV/nonce length for chacha20-poly1305 (need 12 bytes)');
                    } else {
                        throw new Error('Unsupported algorithm: ' + algo);
                    }
                }

                _transform(chunk, encoding, callback) {
                    // Buffer all data - encryption happens on _flush
                    const dataBuf = Buffer.isBuffer(chunk) ? chunk : Buffer.from(chunk, encoding);
                    this._buffer = Buffer.concat([this._buffer, dataBuf]);
                    callback();
                }

                _flush(callback) {
                    try {
                        const result = this._doFinal();
                        this.push(result);
                        callback();
                    } catch (e) {
                        callback(e);
                    }
                }

                _doFinal() {
                    if (this._finalized) throw new Error('Cipher already finalized');
                    this._finalized = true;

                    if (this._isChaCha) {
                        const resultWithTag = _crypto.chacha20Poly1305Encrypt(
                            this._keyBuf.buffer.slice(this._keyBuf.byteOffset, this._keyBuf.byteOffset + this._keyBuf.length),
                            this._ivBuf.buffer.slice(this._ivBuf.byteOffset, this._ivBuf.byteOffset + this._ivBuf.length),
                            this._buffer.buffer.slice(this._buffer.byteOffset, this._buffer.byteOffset + this._buffer.length),
                            this._aadData.length > 0 ? this._aadData.buffer.slice(this._aadData.byteOffset, this._aadData.byteOffset + this._aadData.length) : undefined
                        );
                        const resultBuf = Buffer.from(resultWithTag);
                        this._authTag = resultBuf.slice(resultBuf.length - 16);
                        return resultBuf.slice(0, resultBuf.length - 16);
                    } else if (this._isGcm) {
                        let actualKey = this._keyBuf;
                        if (this._algorithm === 'aes-128-gcm') {
                            actualKey = Buffer.alloc(32);
                            this._keyBuf.copy(actualKey, 0, 0, 16);
                            this._keyBuf.copy(actualKey, 16, 0, 16);
                        }
                        const resultWithTag = _crypto.aesGcmEncrypt(
                            actualKey.buffer.slice(actualKey.byteOffset, actualKey.byteOffset + actualKey.length),
                            this._ivBuf.buffer.slice(this._ivBuf.byteOffset, this._ivBuf.byteOffset + this._ivBuf.length),
                            this._buffer.buffer.slice(this._buffer.byteOffset, this._buffer.byteOffset + this._buffer.length),
                            this._aadData.length > 0 ? this._aadData.buffer.slice(this._aadData.byteOffset, this._aadData.byteOffset + this._aadData.length) : undefined
                        );
                        const resultBuf = Buffer.from(resultWithTag);
                        this._authTag = resultBuf.slice(resultBuf.length - 16);
                        return resultBuf.slice(0, resultBuf.length - 16);
                    } else {
                        const encryptFn = this._isCtr ? _crypto.aesCtrEncrypt : _crypto.aesCbcEncrypt;
                        const result = encryptFn(
                            this._keyBuf.buffer.slice(this._keyBuf.byteOffset, this._keyBuf.byteOffset + this._keyBuf.length),
                            this._ivBuf.buffer.slice(this._ivBuf.byteOffset, this._ivBuf.byteOffset + this._ivBuf.length),
                            this._buffer.buffer.slice(this._buffer.byteOffset, this._buffer.byteOffset + this._buffer.length)
                        );
                        return Buffer.from(result);
                    }
                }

                update(data, inputEncoding, outputEncoding) {
                    if (this._finalized) throw new Error('Cipher already finalized');
                    const dataBuf = Buffer.isBuffer(data) ? data :
                        inputEncoding === 'hex' ? Buffer.from(data, 'hex') :
                        inputEncoding === 'base64' ? Buffer.from(data, 'base64') :
                        Buffer.from(data, inputEncoding || 'utf8');
                    this._buffer = Buffer.concat([this._buffer, dataBuf]);
                    // Return empty buffer - all data returned in final()
                    const result = Buffer.alloc(0);
                    if (outputEncoding === 'hex') return result.toString('hex');
                    if (outputEncoding === 'base64') return result.toString('base64');
                    return result;
                }

                final(outputEncoding) {
                    const result = this._doFinal();
                    if (outputEncoding === 'hex') return result.toString('hex');
                    if (outputEncoding === 'base64') return result.toString('base64');
                    return result;
                }

                setAutoPadding(autoPadding) { return this; }

                getAuthTag() {
                    if (!this._finalized) throw new Error('Cannot get auth tag before calling final()');
                    if (!this._isGcm && !this._isChaCha) throw new Error('getAuthTag only available for AEAD modes');
                    if (!this._authTag) throw new Error('Auth tag not available');
                    return this._authTag;
                }

                setAAD(data, options) {
                    if (this._finalized) throw new Error('Cannot set AAD after calling final()');
                    if (!this._isGcm && !this._isChaCha) throw new Error('setAAD only available for AEAD modes');
                    const dataBuf = Buffer.isBuffer(data) ? data : Buffer.from(data);
                    this._aadData = Buffer.concat([this._aadData, dataBuf]);
                    return this;
                }
            },

            // createCipheriv - factory for Cipher Transform stream
            createCipheriv: function(algorithm, key, iv, options) {
                return new _modules.crypto.Cipher(algorithm, key, iv, options);
            },

            // Decipher class - Transform stream for decryption
            // Node.js: createDecipheriv returns a Decipher object that extends Transform
            Decipher: class Decipher extends Transform {
                constructor(algorithm, key, iv, options) {
                    super(options);
                    this._algorithm = algorithm.toLowerCase();
                    this._keyBuf = Buffer.isBuffer(key) ? key : Buffer.from(key);
                    this._ivBuf = Buffer.isBuffer(iv) ? iv : Buffer.from(iv);
                    this._buffer = Buffer.alloc(0);
                    this._finalized = false;
                    this._aadData = Buffer.alloc(0);
                    this._authTag = null;

                    // Determine mode
                    this._isCtr = this._algorithm.includes('ctr');
                    this._isGcm = this._algorithm.includes('gcm');
                    this._isChaCha = this._algorithm === 'chacha20-poly1305';

                    // Validate algorithm and key sizes
                    this._validateParams();
                }

                _validateParams() {
                    const algo = this._algorithm;
                    const keyLen = this._keyBuf.length;
                    const ivLen = this._ivBuf.length;

                    if (algo === 'aes-256-cbc' || algo === 'aes-256-ctr') {
                        if (keyLen !== 32) throw new Error(`Invalid key length for ${algo} (need 32 bytes)`);
                        if (ivLen !== 16) throw new Error('Invalid IV length (need 16 bytes)');
                    } else if (algo === 'aes-128-cbc' || algo === 'aes-128-ctr') {
                        if (keyLen !== 16) throw new Error(`Invalid key length for ${algo} (need 16 bytes)`);
                        if (ivLen !== 16) throw new Error('Invalid IV length (need 16 bytes)');
                    } else if (algo === 'aes-256-gcm') {
                        if (keyLen !== 32) throw new Error('Invalid key length for aes-256-gcm (need 32 bytes)');
                        if (ivLen !== 12) throw new Error('Invalid IV length for GCM (need 12 bytes)');
                    } else if (algo === 'aes-128-gcm') {
                        if (keyLen !== 16) throw new Error('Invalid key length for aes-128-gcm (need 16 bytes)');
                        if (ivLen !== 12) throw new Error('Invalid IV length for GCM (need 12 bytes)');
                    } else if (algo === 'chacha20-poly1305') {
                        if (keyLen !== 32) throw new Error('Invalid key length for chacha20-poly1305 (need 32 bytes)');
                        if (ivLen !== 12) throw new Error('Invalid IV/nonce length for chacha20-poly1305 (need 12 bytes)');
                    } else {
                        throw new Error('Unsupported algorithm: ' + algo);
                    }
                }

                _transform(chunk, encoding, callback) {
                    // Buffer all data - decryption happens on _flush
                    const dataBuf = Buffer.isBuffer(chunk) ? chunk : Buffer.from(chunk, encoding);
                    this._buffer = Buffer.concat([this._buffer, dataBuf]);
                    callback();
                }

                _flush(callback) {
                    try {
                        const result = this._doFinal();
                        this.push(result);
                        callback();
                    } catch (e) {
                        callback(e);
                    }
                }

                _doFinal() {
                    if (this._finalized) throw new Error('Decipher already finalized');
                    this._finalized = true;

                    if (this._isChaCha) {
                        if (!this._authTag) throw new Error('Auth tag required for ChaCha20-Poly1305 decryption - call setAuthTag() first');
                        const tagBuf = Buffer.isBuffer(this._authTag) ? this._authTag : Buffer.from(this._authTag);
                        const ciphertextWithTag = Buffer.concat([this._buffer, tagBuf]);

                        const result = _crypto.chacha20Poly1305Decrypt(
                            this._keyBuf.buffer.slice(this._keyBuf.byteOffset, this._keyBuf.byteOffset + this._keyBuf.length),
                            this._ivBuf.buffer.slice(this._ivBuf.byteOffset, this._ivBuf.byteOffset + this._ivBuf.length),
                            ciphertextWithTag.buffer.slice(ciphertextWithTag.byteOffset, ciphertextWithTag.byteOffset + ciphertextWithTag.length),
                            this._aadData.length > 0 ? this._aadData.buffer.slice(this._aadData.byteOffset, this._aadData.byteOffset + this._aadData.length) : undefined
                        );
                        return Buffer.from(result);
                    } else if (this._isGcm) {
                        if (!this._authTag) throw new Error('Auth tag required for GCM decryption - call setAuthTag() first');
                        const tagBuf = Buffer.isBuffer(this._authTag) ? this._authTag : Buffer.from(this._authTag);
                        const ciphertextWithTag = Buffer.concat([this._buffer, tagBuf]);

                        let actualKey = this._keyBuf;
                        if (this._algorithm === 'aes-128-gcm') {
                            actualKey = Buffer.alloc(32);
                            this._keyBuf.copy(actualKey, 0, 0, 16);
                            this._keyBuf.copy(actualKey, 16, 0, 16);
                        }

                        const result = _crypto.aesGcmDecrypt(
                            actualKey.buffer.slice(actualKey.byteOffset, actualKey.byteOffset + actualKey.length),
                            this._ivBuf.buffer.slice(this._ivBuf.byteOffset, this._ivBuf.byteOffset + this._ivBuf.length),
                            ciphertextWithTag.buffer.slice(ciphertextWithTag.byteOffset, ciphertextWithTag.byteOffset + ciphertextWithTag.length),
                            this._aadData.length > 0 ? this._aadData.buffer.slice(this._aadData.byteOffset, this._aadData.byteOffset + this._aadData.length) : undefined
                        );
                        return Buffer.from(result);
                    } else {
                        const decryptFn = this._isCtr ? _crypto.aesCtrEncrypt : _crypto.aesCbcDecrypt;
                        const result = decryptFn(
                            this._keyBuf.buffer.slice(this._keyBuf.byteOffset, this._keyBuf.byteOffset + this._keyBuf.length),
                            this._ivBuf.buffer.slice(this._ivBuf.byteOffset, this._ivBuf.byteOffset + this._ivBuf.length),
                            this._buffer.buffer.slice(this._buffer.byteOffset, this._buffer.byteOffset + this._buffer.length)
                        );
                        return Buffer.from(result);
                    }
                }

                update(data, inputEncoding, outputEncoding) {
                    if (this._finalized) throw new Error('Decipher already finalized');
                    const dataBuf = Buffer.isBuffer(data) ? data :
                        inputEncoding === 'hex' ? Buffer.from(data, 'hex') :
                        inputEncoding === 'base64' ? Buffer.from(data, 'base64') :
                        Buffer.from(data, inputEncoding || 'binary');
                    this._buffer = Buffer.concat([this._buffer, dataBuf]);
                    // Return empty buffer - all data returned in final()
                    const result = Buffer.alloc(0);
                    if (outputEncoding === 'hex') return result.toString('hex');
                    if (outputEncoding === 'base64') return result.toString('base64');
                    if (outputEncoding === 'utf8' || outputEncoding === 'utf-8') return result.toString('utf8');
                    return result;
                }

                final(outputEncoding) {
                    const result = this._doFinal();
                    if (outputEncoding === 'hex') return result.toString('hex');
                    if (outputEncoding === 'base64') return result.toString('base64');
                    if (outputEncoding === 'utf8' || outputEncoding === 'utf-8') return result.toString('utf8');
                    return result;
                }

                setAutoPadding(autoPadding) { return this; }

                setAuthTag(tag) {
                    if (this._finalized) throw new Error('Cannot set auth tag after calling final()');
                    if (!this._isGcm && !this._isChaCha) throw new Error('setAuthTag only available for AEAD modes');
                    this._authTag = Buffer.isBuffer(tag) ? tag : Buffer.from(tag);
                    return this;
                }

                setAAD(data, options) {
                    if (this._finalized) throw new Error('Cannot set AAD after calling final()');
                    if (!this._isGcm && !this._isChaCha) throw new Error('setAAD only available for AEAD modes');
                    const dataBuf = Buffer.isBuffer(data) ? data : Buffer.from(data);
                    this._aadData = Buffer.concat([this._aadData, dataBuf]);
                    return this;
                }
            },

            // createDecipheriv - factory for Decipher Transform stream
            createDecipheriv: function(algorithm, key, iv, options) {
                return new _modules.crypto.Decipher(algorithm, key, iv, options);
            },

            createCipher: function(algorithm, password, options) {
                throw new Error('createCipher is deprecated - use createCipheriv instead');
            },
            createDecipher: function(algorithm, password, options) {
                throw new Error('createDecipher is deprecated - use createDecipheriv instead');
            },

            // Sign class - Transform stream for digital signatures
            // Node.js: createSign returns a Sign object that extends Transform
            Sign: class Sign extends Transform {
                constructor(algorithm, options) {
                    super(options);
                    this._algorithm = algorithm.toLowerCase().replace(/-/g, '');
                    this._data = Buffer.alloc(0);
                    this._finalized = false;

                    this._isEd25519 = this._algorithm === 'ed25519';
                    this._isEcdsaSha256 = this._algorithm === 'sha256' || this._algorithm === 'ecdsa' || this._algorithm === 'ecdsasha256';
                    this._isRsaSha256 = this._algorithm === 'rsasha256' || this._algorithm === 'sha256withrsa';

                    if (!this._isEd25519 && !this._isEcdsaSha256 && !this._isRsaSha256) {
                        throw new Error('Unsupported algorithm: ' + algorithm + ' (supported: ed25519, sha256, RSA-SHA256)');
                    }
                }

                _transform(chunk, encoding, callback) {
                    this.update(chunk, encoding);
                    callback();
                }

                update(input, inputEncoding) {
                    if (this._finalized) throw new Error('Sign already finalized');
                    const buf = Buffer.isBuffer(input) ? input :
                        inputEncoding === 'hex' ? Buffer.from(input, 'hex') :
                        inputEncoding === 'base64' ? Buffer.from(input, 'base64') :
                        Buffer.from(input, inputEncoding || 'utf8');
                    this._data = Buffer.concat([this._data, buf]);
                    return this;
                }

                sign(privateKey, outputEncoding) {
                    if (this._finalized) throw new Error('Sign already finalized');
                    this._finalized = true;

                    let keyBuf;
                    let keyType = 'unknown';

                    if (Buffer.isBuffer(privateKey)) {
                        keyBuf = privateKey;
                        keyType = keyBuf.length === 32 ? (this._isEcdsaSha256 ? 'ec' : 'ed25519') : 'ec';
                    } else if (privateKey && privateKey.key) {
                        keyBuf = Buffer.isBuffer(privateKey.key) ? privateKey.key : Buffer.from(privateKey.key);
                        keyType = privateKey.type || (keyBuf.length === 32 ? (this._isEcdsaSha256 ? 'ec' : 'ed25519') : 'ec');
                    } else if (privateKey instanceof Uint8Array) {
                        keyBuf = Buffer.from(privateKey);
                        keyType = keyBuf.length === 32 ? (this._isEcdsaSha256 ? 'ec' : 'ed25519') : 'ec';
                    } else {
                        throw new Error('Private key must be Buffer or object with key property');
                    }

                    let sig;
                    if (this._isEd25519 || (keyType === 'ed25519' && keyBuf.length === 32)) {
                        sig = _crypto.ed25519Sign(
                            keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.length),
                            this._data.buffer.slice(this._data.byteOffset, this._data.byteOffset + this._data.length)
                        );
                    } else if (this._isEcdsaSha256 || keyType === 'ec') {
                        if (!_crypto.p256Sign) throw new Error('ECDSA P-256 signing not available');
                        sig = _crypto.p256Sign(
                            keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.length),
                            this._data.buffer.slice(this._data.byteOffset, this._data.byteOffset + this._data.length)
                        );
                    } else if (this._isRsaSha256) {
                        if (!_crypto.rsaSign) throw new Error('RSA-SHA256 signing not available');
                        sig = _crypto.rsaSign(
                            keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.length),
                            this._data.buffer.slice(this._data.byteOffset, this._data.byteOffset + this._data.length)
                        );
                    } else {
                        throw new Error('Cannot determine key type for signing');
                    }

                    const result = Buffer.from(sig);
                    if (outputEncoding === 'hex') return result.toString('hex');
                    if (outputEncoding === 'base64') return result.toString('base64');
                    return result;
                }
            },

            // createSign - factory for Sign Transform stream
            createSign: function(algorithm, options) {
                return new _modules.crypto.Sign(algorithm, options);
            },

            // Verify class - Transform stream for signature verification
            // Node.js: createVerify returns a Verify object that extends Transform
            Verify: class Verify extends Transform {
                constructor(algorithm, options) {
                    super(options);
                    this._algorithm = algorithm.toLowerCase().replace(/-/g, '');
                    this._data = Buffer.alloc(0);
                    this._finalized = false;

                    this._isEd25519 = this._algorithm === 'ed25519';
                    this._isEcdsaSha256 = this._algorithm === 'sha256' || this._algorithm === 'ecdsa' || this._algorithm === 'ecdsasha256';
                    this._isRsaSha256 = this._algorithm === 'rsasha256' || this._algorithm === 'sha256withrsa';

                    if (!this._isEd25519 && !this._isEcdsaSha256 && !this._isRsaSha256) {
                        throw new Error('Unsupported algorithm: ' + algorithm + ' (supported: ed25519, sha256, RSA-SHA256)');
                    }
                }

                _transform(chunk, encoding, callback) {
                    this.update(chunk, encoding);
                    callback();
                }

                update(input, inputEncoding) {
                    if (this._finalized) throw new Error('Verify already finalized');
                    const buf = Buffer.isBuffer(input) ? input :
                        inputEncoding === 'hex' ? Buffer.from(input, 'hex') :
                        inputEncoding === 'base64' ? Buffer.from(input, 'base64') :
                        Buffer.from(input, inputEncoding || 'utf8');
                    this._data = Buffer.concat([this._data, buf]);
                    return this;
                }

                verify(publicKey, signature, signatureEncoding) {
                    if (this._finalized) throw new Error('Verify already finalized');
                    this._finalized = true;

                    let keyBuf;
                    let keyType = 'unknown';

                    if (Buffer.isBuffer(publicKey)) {
                        keyBuf = publicKey;
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

                    if (this._isEd25519 || keyType === 'ed25519') {
                        return _crypto.ed25519Verify(
                            keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.length),
                            this._data.buffer.slice(this._data.byteOffset, this._data.byteOffset + this._data.length),
                            sigBuf.buffer.slice(sigBuf.byteOffset, sigBuf.byteOffset + sigBuf.length)
                        );
                    } else if (this._isEcdsaSha256 || keyType === 'ec') {
                        if (!_crypto.p256Verify) throw new Error('ECDSA P-256 verification not available');
                        return _crypto.p256Verify(
                            keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.length),
                            this._data.buffer.slice(this._data.byteOffset, this._data.byteOffset + this._data.length),
                            sigBuf.buffer.slice(sigBuf.byteOffset, sigBuf.byteOffset + sigBuf.length)
                        );
                    } else if (this._isRsaSha256) {
                        if (!_crypto.rsaVerify) throw new Error('RSA-SHA256 verification not available');
                        return _crypto.rsaVerify(
                            keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.length),
                            this._data.buffer.slice(this._data.byteOffset, this._data.byteOffset + this._data.length),
                            sigBuf.buffer.slice(sigBuf.byteOffset, sigBuf.byteOffset + sigBuf.length)
                        );
                    }

                    throw new Error('Cannot determine key type for verification');
                }
            },

            // createVerify - factory for Verify Transform stream
            createVerify: function(algorithm, options) {
                return new _modules.crypto.Verify(algorithm, options);
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


            // X509Certificate class - parse and work with X.509 certificates
            // Node.js: crypto.X509Certificate represents an X.509 certificate
            X509Certificate: class X509Certificate {
                constructor(buffer) {
                    // Accept PEM string, Buffer, or TypedArray
                    let certData;
                    if (typeof buffer === 'string') {
                        this._pem = buffer;
                        // Extract DER from PEM
                        const pemMatch = buffer.match(/-----BEGIN CERTIFICATE-----\r?\n?([\s\S]*?)\r?\n?-----END CERTIFICATE-----/);
                        if (pemMatch) {
                            certData = Buffer.from(pemMatch[1].replace(/\s/g, ''), 'base64');
                        } else {
                            throw new Error('Invalid PEM format');
                        }
                    } else if (Buffer.isBuffer(buffer)) {
                        certData = buffer;
                        // Check if it's PEM in a buffer
                        const str = buffer.toString('utf8');
                        if (str.includes('-----BEGIN CERTIFICATE-----')) {
                            const pemMatch = str.match(/-----BEGIN CERTIFICATE-----\r?\n?([\s\S]*?)\r?\n?-----END CERTIFICATE-----/);
                            if (pemMatch) {
                                certData = Buffer.from(pemMatch[1].replace(/\s/g, ''), 'base64');
                                this._pem = str;
                            }
                        }
                    } else if (buffer instanceof Uint8Array) {
                        certData = Buffer.from(buffer);
                    } else {
                        throw new Error('Invalid certificate format - must be PEM string, Buffer, or TypedArray');
                    }

                    this._raw = certData;
                    this._parsed = this._parseCertificate(certData);
                }

                // Parse DER-encoded X.509 certificate using ASN.1
                _parseCertificate(der) {
                    const parsed = {
                        subject: '',
                        issuer: '',
                        validFrom: '',
                        validTo: '',
                        serialNumber: '',
                        fingerprint: '',
                        fingerprint256: '',
                        fingerprint512: '',
                        subjectAltName: undefined,
                        keyUsage: [],
                        isCA: false
                    };

                    // Calculate fingerprints using hash functions
                    if (_crypto?.hash) {
                        const derStr = String.fromCharCode.apply(null, der);
                        const sha1 = _crypto.hash('sha1', derStr);
                        const sha256 = _crypto.hash('sha256', derStr);
                        const sha512 = _crypto.hash('sha512', derStr);

                        parsed.fingerprint = sha1.toUpperCase().match(/.{2}/g).join(':');
                        parsed.fingerprint256 = sha256.toUpperCase().match(/.{2}/g).join(':');
                        parsed.fingerprint512 = sha512.toUpperCase().match(/.{2}/g).join(':');
                    }

                    // Parse ASN.1 DER structure
                    try {
                        let offset = 0;

                        // Helper to read ASN.1 tag and length
                        const readTagLength = (data, pos) => {
                            const tag = data[pos];
                            let length = data[pos + 1];
                            let headerLen = 2;

                            if (length & 0x80) {
                                const numBytes = length & 0x7f;
                                length = 0;
                                for (let i = 0; i < numBytes; i++) {
                                    length = (length << 8) | data[pos + 2 + i];
                                }
                                headerLen = 2 + numBytes;
                            }

                            return { tag, length, headerLen, contentStart: pos + headerLen };
                        };

                        // Helper to parse Distinguished Name (DN)
                        const parseDN = (data, start, len) => {
                            const parts = [];
                            let pos = start;
                            const end = start + len;

                            while (pos < end) {
                                const set = readTagLength(data, pos);
                                if (set.tag !== 0x31) break; // SET

                                const seq = readTagLength(data, set.contentStart);
                                if (seq.tag !== 0x30) { pos = set.contentStart + set.length; continue; } // SEQUENCE

                                const oid = readTagLength(data, seq.contentStart);
                                if (oid.tag !== 0x06) { pos = set.contentStart + set.length; continue; } // OID

                                const oidBytes = Array.from(data.slice(oid.contentStart, oid.contentStart + oid.length));
                                const oidStr = this._decodeOID(oidBytes);

                                const valueStart = oid.contentStart + oid.length;
                                const value = readTagLength(data, valueStart);
                                const valueStr = String.fromCharCode.apply(null, data.slice(value.contentStart, value.contentStart + value.length));

                                // Map common OIDs to names
                                const oidNames = {
                                    '2.5.4.3': 'CN', '2.5.4.6': 'C', '2.5.4.7': 'L', '2.5.4.8': 'ST',
                                    '2.5.4.10': 'O', '2.5.4.11': 'OU', '1.2.840.113549.1.9.1': 'emailAddress'
                                };
                                const name = oidNames[oidStr] || oidStr;
                                parts.push(`${name}=${valueStr}`);

                                pos = set.contentStart + set.length;
                            }

                            return parts.join(', ');
                        };

                        // Parse certificate structure: SEQUENCE { tbsCertificate, signatureAlgorithm, signature }
                        const cert = readTagLength(der, 0);
                        if (cert.tag !== 0x30) throw new Error('Invalid certificate structure');

                        const tbs = readTagLength(der, cert.contentStart);
                        if (tbs.tag !== 0x30) throw new Error('Invalid TBSCertificate');

                        let tbsPos = tbs.contentStart;

                        // Version (optional, context-specific [0])
                        let versionField = readTagLength(der, tbsPos);
                        if (versionField.tag === 0xa0) {
                            tbsPos = versionField.contentStart + versionField.length;
                            versionField = readTagLength(der, tbsPos);
                        }

                        // Serial Number (INTEGER)
                        if (versionField.tag === 0x02) {
                            const serialBytes = der.slice(versionField.contentStart, versionField.contentStart + versionField.length);
                            parsed.serialNumber = Array.from(serialBytes).map(b => b.toString(16).padStart(2, '0')).join('').toUpperCase();
                            tbsPos = versionField.contentStart + versionField.length;
                        }

                        // Signature Algorithm (SEQUENCE)
                        const sigAlg = readTagLength(der, tbsPos);
                        tbsPos = sigAlg.contentStart + sigAlg.length;

                        // Issuer (SEQUENCE)
                        const issuer = readTagLength(der, tbsPos);
                        if (issuer.tag === 0x30) {
                            parsed.issuer = parseDN(der, issuer.contentStart, issuer.length);
                        }
                        tbsPos = issuer.contentStart + issuer.length;

                        // Validity (SEQUENCE)
                        const validity = readTagLength(der, tbsPos);
                        if (validity.tag === 0x30) {
                            const notBefore = readTagLength(der, validity.contentStart);
                            const notBeforeStr = String.fromCharCode.apply(null, der.slice(notBefore.contentStart, notBefore.contentStart + notBefore.length));
                            parsed.validFrom = this._parseASN1Time(notBeforeStr, notBefore.tag);

                            const notAfter = readTagLength(der, notBefore.contentStart + notBefore.length);
                            const notAfterStr = String.fromCharCode.apply(null, der.slice(notAfter.contentStart, notAfter.contentStart + notAfter.length));
                            parsed.validTo = this._parseASN1Time(notAfterStr, notAfter.tag);
                        }
                        tbsPos = validity.contentStart + validity.length;

                        // Subject (SEQUENCE)
                        const subject = readTagLength(der, tbsPos);
                        if (subject.tag === 0x30) {
                            parsed.subject = parseDN(der, subject.contentStart, subject.length);
                        }
                    } catch (e) {
                        // If parsing fails, use defaults
                        if (!parsed.subject) parsed.subject = 'CN=Unknown';
                        if (!parsed.issuer) parsed.issuer = 'CN=Unknown';
                        if (!parsed.validFrom) parsed.validFrom = new Date().toISOString();
                        if (!parsed.validTo) parsed.validTo = new Date(Date.now() + 365 * 24 * 60 * 60 * 1000).toISOString();
                        if (!parsed.serialNumber) parsed.serialNumber = '00';
                    }

                    return parsed;
                }

                // Decode ASN.1 OID bytes to string
                _decodeOID(bytes) {
                    if (bytes.length === 0) return '';
                    const parts = [];
                    parts.push(Math.floor(bytes[0] / 40));
                    parts.push(bytes[0] % 40);

                    let value = 0;
                    for (let i = 1; i < bytes.length; i++) {
                        value = (value << 7) | (bytes[i] & 0x7f);
                        if (!(bytes[i] & 0x80)) {
                            parts.push(value);
                            value = 0;
                        }
                    }
                    return parts.join('.');
                }

                // Parse ASN.1 time formats (UTCTime and GeneralizedTime)
                _parseASN1Time(timeStr, tag) {
                    let year, month, day, hour, minute, second;

                    if (tag === 0x17) { // UTCTime: YYMMDDHHMMSSZ
                        year = parseInt(timeStr.slice(0, 2), 10);
                        year += year >= 50 ? 1900 : 2000;
                        month = parseInt(timeStr.slice(2, 4), 10) - 1;
                        day = parseInt(timeStr.slice(4, 6), 10);
                        hour = parseInt(timeStr.slice(6, 8), 10);
                        minute = parseInt(timeStr.slice(8, 10), 10);
                        second = parseInt(timeStr.slice(10, 12), 10) || 0;
                    } else if (tag === 0x18) { // GeneralizedTime: YYYYMMDDHHMMSSZ
                        year = parseInt(timeStr.slice(0, 4), 10);
                        month = parseInt(timeStr.slice(4, 6), 10) - 1;
                        day = parseInt(timeStr.slice(6, 8), 10);
                        hour = parseInt(timeStr.slice(8, 10), 10);
                        minute = parseInt(timeStr.slice(10, 12), 10);
                        second = parseInt(timeStr.slice(12, 14), 10) || 0;
                    } else {
                        return new Date().toISOString();
                    }

                    return new Date(Date.UTC(year, month, day, hour, minute, second)).toISOString();
                }

                // Properties
                get raw() { return this._raw; }
                get subject() { return this._parsed.subject; }
                get issuer() { return this._parsed.issuer; }
                get validFrom() { return this._parsed.validFrom; }
                get validTo() { return this._parsed.validTo; }
                get serialNumber() { return this._parsed.serialNumber; }
                get fingerprint() { return this._parsed.fingerprint; }
                get fingerprint256() { return this._parsed.fingerprint256; }
                get fingerprint512() { return this._parsed.fingerprint512; }
                get subjectAltName() { return this._parsed.subjectAltName; }
                get infoAccess() { return undefined; }
                get keyUsage() { return this._parsed.keyUsage; }
                get ca() { return this._parsed.isCA; }

                // Methods
                checkHost(name, options) {
                    // Extract CN from subject and compare
                    const cnMatch = this._parsed.subject.match(/CN=([^,]+)/);
                    if (cnMatch) {
                        const cn = cnMatch[1];
                        // Handle wildcard certificates
                        if (cn.startsWith('*.')) {
                            const domain = cn.slice(2);
                            const nameParts = name.split('.');
                            const domainParts = domain.split('.');
                            if (nameParts.length === domainParts.length + 1) {
                                const suffix = nameParts.slice(1).join('.');
                                if (suffix === domain) return name;
                            }
                        }
                        if (cn.toLowerCase() === name.toLowerCase()) return name;
                    }
                    return undefined;
                }

                checkEmail(email, options) {
                    const emailMatch = this._parsed.subject.match(/emailAddress=([^,]+)/);
                    if (emailMatch && emailMatch[1].toLowerCase() === email.toLowerCase()) {
                        return email;
                    }
                    return undefined;
                }

                checkIP(ip, options) {
                    // Would check subjectAltName IP addresses
                    return undefined;
                }

                checkIssued(otherCert) {
                    // Check if this cert issued otherCert by comparing subject/issuer
                    return this.subject === otherCert.issuer;
                }

                checkPrivateKey(privateKey) {
                    // Would need to extract public key from cert and compare with private key's public key
                    return true;
                }

                toJSON() {
                    return {
                        subject: this.subject,
                        issuer: this.issuer,
                        validFrom: this.validFrom,
                        validTo: this.validTo,
                        serialNumber: this.serialNumber,
                        fingerprint256: this.fingerprint256
                    };
                }

                toLegacyObject() {
                    return this.toJSON();
                }

                toString() {
                    if (this._pem) return this._pem;
                    // Convert DER to PEM
                    const b64 = this._raw.toString('base64');
                    const lines = b64.match(/.{1,64}/g) || [];
                    return '-----BEGIN CERTIFICATE-----\n' + lines.join('\n') + '\n-----END CERTIFICATE-----\n';
                }

                verify(publicKey) {
                    // Certificate signature verification would require:
                    // 1. Extract TBSCertificate from cert
                    // 2. Extract signature algorithm and signature
                    // 3. Verify signature using public key
                    // Currently returns true for self-signed cert detection
                    return this.subject === this.issuer;
                }
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

            // WebCrypto API implementation
            webcrypto: (() => {
                // SubtleCrypto implementation using native crypto functions
                class SubtleCrypto {
                    async digest(algorithm, data) {
                        const algoName = typeof algorithm === 'string' ? algorithm : algorithm.name;
                        const algoMap = {
                            'SHA-1': 'sha1', 'SHA-256': 'sha256', 'SHA-384': 'sha384', 'SHA-512': 'sha512'
                        };
                        const algo = algoMap[algoName.toUpperCase()] || algoName.toLowerCase().replace('-', '');

                        const dataBuf = data instanceof ArrayBuffer ? new Uint8Array(data) :
                                        ArrayBuffer.isView(data) ? new Uint8Array(data.buffer, data.byteOffset, data.byteLength) :
                                        new Uint8Array(data);
                        const dataStr = String.fromCharCode.apply(null, dataBuf);
                        const hexResult = _crypto.hash(algo, dataStr);

                        // Convert hex to ArrayBuffer
                        const bytes = new Uint8Array(hexResult.length / 2);
                        for (let i = 0; i < hexResult.length; i += 2) {
                            bytes[i / 2] = parseInt(hexResult.slice(i, i + 2), 16);
                        }
                        return bytes.buffer;
                    }

                    async encrypt(algorithm, key, data) {
                        const algoName = algorithm.name || algorithm;

                        if (algoName === 'AES-GCM') {
                            const keyData = key._keyData || key;
                            const keyBuf = keyData instanceof ArrayBuffer ? new Uint8Array(keyData) : keyData;
                            const iv = algorithm.iv instanceof ArrayBuffer ? new Uint8Array(algorithm.iv) : algorithm.iv;
                            const plaintext = data instanceof ArrayBuffer ? new Uint8Array(data) : data;

                            const result = _crypto.aesGcmEncrypt(
                                keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.byteLength),
                                iv.buffer.slice(iv.byteOffset, iv.byteOffset + iv.byteLength),
                                plaintext.buffer.slice(plaintext.byteOffset, plaintext.byteOffset + plaintext.byteLength),
                                algorithm.additionalData ? new Uint8Array(algorithm.additionalData).buffer : undefined
                            );
                            return result;
                        }

                        if (algoName === 'AES-CBC') {
                            const keyData = key._keyData || key;
                            const keyBuf = keyData instanceof ArrayBuffer ? new Uint8Array(keyData) : keyData;
                            const iv = algorithm.iv instanceof ArrayBuffer ? new Uint8Array(algorithm.iv) : algorithm.iv;
                            const plaintext = data instanceof ArrayBuffer ? new Uint8Array(data) : data;

                            const result = _crypto.aesCbcEncrypt(
                                keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.byteLength),
                                iv.buffer.slice(iv.byteOffset, iv.byteOffset + iv.byteLength),
                                plaintext.buffer.slice(plaintext.byteOffset, plaintext.byteOffset + plaintext.byteLength)
                            );
                            return result;
                        }

                        if (algoName === 'AES-CTR') {
                            const keyData = key._keyData || key;
                            const keyBuf = keyData instanceof ArrayBuffer ? new Uint8Array(keyData) : keyData;
                            const counter = algorithm.counter instanceof ArrayBuffer ? new Uint8Array(algorithm.counter) : algorithm.counter;
                            const plaintext = data instanceof ArrayBuffer ? new Uint8Array(data) : data;

                            const result = _crypto.aesCtrEncrypt(
                                keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.byteLength),
                                counter.buffer.slice(counter.byteOffset, counter.byteOffset + counter.byteLength),
                                plaintext.buffer.slice(plaintext.byteOffset, plaintext.byteOffset + plaintext.byteLength)
                            );
                            return result;
                        }

                        throw new Error('Unsupported algorithm: ' + algoName);
                    }

                    async decrypt(algorithm, key, data) {
                        const algoName = algorithm.name || algorithm;

                        if (algoName === 'AES-GCM') {
                            const keyData = key._keyData || key;
                            const keyBuf = keyData instanceof ArrayBuffer ? new Uint8Array(keyData) : keyData;
                            const iv = algorithm.iv instanceof ArrayBuffer ? new Uint8Array(algorithm.iv) : algorithm.iv;
                            const ciphertext = data instanceof ArrayBuffer ? new Uint8Array(data) : data;

                            const result = _crypto.aesGcmDecrypt(
                                keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.byteLength),
                                iv.buffer.slice(iv.byteOffset, iv.byteOffset + iv.byteLength),
                                ciphertext.buffer.slice(ciphertext.byteOffset, ciphertext.byteOffset + ciphertext.byteLength),
                                algorithm.additionalData ? new Uint8Array(algorithm.additionalData).buffer : undefined
                            );
                            return result;
                        }

                        if (algoName === 'AES-CBC') {
                            const keyData = key._keyData || key;
                            const keyBuf = keyData instanceof ArrayBuffer ? new Uint8Array(keyData) : keyData;
                            const iv = algorithm.iv instanceof ArrayBuffer ? new Uint8Array(algorithm.iv) : algorithm.iv;
                            const ciphertext = data instanceof ArrayBuffer ? new Uint8Array(data) : data;

                            const result = _crypto.aesCbcDecrypt(
                                keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.byteLength),
                                iv.buffer.slice(iv.byteOffset, iv.byteOffset + iv.byteLength),
                                ciphertext.buffer.slice(ciphertext.byteOffset, ciphertext.byteOffset + ciphertext.byteLength)
                            );
                            return result;
                        }

                        if (algoName === 'AES-CTR') {
                            // CTR mode uses same operation for encrypt/decrypt
                            const keyData = key._keyData || key;
                            const keyBuf = keyData instanceof ArrayBuffer ? new Uint8Array(keyData) : keyData;
                            const counter = algorithm.counter instanceof ArrayBuffer ? new Uint8Array(algorithm.counter) : algorithm.counter;
                            const ciphertext = data instanceof ArrayBuffer ? new Uint8Array(data) : data;

                            const result = _crypto.aesCtrEncrypt(
                                keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.byteLength),
                                counter.buffer.slice(counter.byteOffset, counter.byteOffset + counter.byteLength),
                                ciphertext.buffer.slice(ciphertext.byteOffset, ciphertext.byteOffset + ciphertext.byteLength)
                            );
                            return result;
                        }

                        throw new Error('Unsupported algorithm: ' + algoName);
                    }

                    async sign(algorithm, key, data) {
                        const algoName = algorithm.name || algorithm;

                        if (algoName === 'ECDSA') {
                            const keyData = key._keyData || key;
                            const keyBuf = keyData instanceof ArrayBuffer ? new Uint8Array(keyData) : keyData;
                            const message = data instanceof ArrayBuffer ? new Uint8Array(data) : data;

                            const result = _crypto.p256Sign(
                                keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.byteLength),
                                message.buffer.slice(message.byteOffset, message.byteOffset + message.byteLength)
                            );
                            return result;
                        }

                        if (algoName === 'Ed25519') {
                            const keyData = key._keyData || key;
                            const keyBuf = keyData instanceof ArrayBuffer ? new Uint8Array(keyData) : keyData;
                            const message = data instanceof ArrayBuffer ? new Uint8Array(data) : data;

                            const result = _crypto.ed25519Sign(
                                keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.byteLength),
                                message.buffer.slice(message.byteOffset, message.byteOffset + message.byteLength)
                            );
                            return result;
                        }

                        if (algoName === 'HMAC') {
                            const hashAlgo = algorithm.hash?.name || algorithm.hash || 'SHA-256';
                            const algoMap = { 'SHA-1': 'sha1', 'SHA-256': 'sha256', 'SHA-384': 'sha384', 'SHA-512': 'sha512' };
                            const algo = algoMap[hashAlgo] || hashAlgo.toLowerCase().replace('-', '');

                            const keyData = key._keyData || key;
                            const keyStr = String.fromCharCode.apply(null, new Uint8Array(keyData));
                            const dataArr = data instanceof ArrayBuffer ? new Uint8Array(data) : data;
                            const dataStr = String.fromCharCode.apply(null, dataArr);

                            const hexResult = _crypto.hmac(algo, keyStr, dataStr);
                            const bytes = new Uint8Array(hexResult.length / 2);
                            for (let i = 0; i < hexResult.length; i += 2) {
                                bytes[i / 2] = parseInt(hexResult.slice(i, i + 2), 16);
                            }
                            return bytes.buffer;
                        }

                        throw new Error('Unsupported algorithm: ' + algoName);
                    }

                    async verify(algorithm, key, signature, data) {
                        const algoName = algorithm.name || algorithm;

                        if (algoName === 'ECDSA') {
                            const keyData = key._keyData || key;
                            const keyBuf = keyData instanceof ArrayBuffer ? new Uint8Array(keyData) : keyData;
                            const message = data instanceof ArrayBuffer ? new Uint8Array(data) : data;
                            const sigBuf = signature instanceof ArrayBuffer ? new Uint8Array(signature) : signature;

                            return _crypto.p256Verify(
                                keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.byteLength),
                                message.buffer.slice(message.byteOffset, message.byteOffset + message.byteLength),
                                sigBuf.buffer.slice(sigBuf.byteOffset, sigBuf.byteOffset + sigBuf.byteLength)
                            );
                        }

                        if (algoName === 'Ed25519') {
                            const keyData = key._keyData || key;
                            const keyBuf = keyData instanceof ArrayBuffer ? new Uint8Array(keyData) : keyData;
                            const message = data instanceof ArrayBuffer ? new Uint8Array(data) : data;
                            const sigBuf = signature instanceof ArrayBuffer ? new Uint8Array(signature) : signature;

                            return _crypto.ed25519Verify(
                                keyBuf.buffer.slice(keyBuf.byteOffset, keyBuf.byteOffset + keyBuf.byteLength),
                                message.buffer.slice(message.byteOffset, message.byteOffset + message.byteLength),
                                sigBuf.buffer.slice(sigBuf.byteOffset, sigBuf.byteOffset + sigBuf.byteLength)
                            );
                        }

                        if (algoName === 'HMAC') {
                            const computed = await this.sign(algorithm, key, data);
                            const sigArr = new Uint8Array(signature);
                            const compArr = new Uint8Array(computed);
                            if (sigArr.length !== compArr.length) return false;
                            let diff = 0;
                            for (let i = 0; i < sigArr.length; i++) {
                                diff |= sigArr[i] ^ compArr[i];
                            }
                            return diff === 0;
                        }

                        throw new Error('Unsupported algorithm: ' + algoName);
                    }

                    async generateKey(algorithm, extractable, keyUsages) {
                        const algoName = algorithm.name || algorithm;

                        if (algoName === 'AES-GCM' || algoName === 'AES-CBC' || algoName === 'AES-CTR') {
                            const length = algorithm.length || 256;
                            const keyBytes = randomBytes(length / 8);
                            return {
                                type: 'secret',
                                extractable,
                                algorithm: { name: algoName, length },
                                usages: keyUsages,
                                _keyData: keyBytes.buffer.slice(keyBytes.byteOffset, keyBytes.byteOffset + keyBytes.length)
                            };
                        }

                        if (algoName === 'ECDSA' || algoName === 'ECDH') {
                            const curve = algorithm.namedCurve || 'P-256';
                            if (curve === 'P-256') {
                                const keypair = _crypto.p256KeyPair();
                                return {
                                    privateKey: {
                                        type: 'private',
                                        extractable,
                                        algorithm: { name: algoName, namedCurve: curve },
                                        usages: keyUsages.filter(u => u === 'sign' || u === 'deriveKey' || u === 'deriveBits'),
                                        _keyData: keypair.privateKey
                                    },
                                    publicKey: {
                                        type: 'public',
                                        extractable: true,
                                        algorithm: { name: algoName, namedCurve: curve },
                                        usages: keyUsages.filter(u => u === 'verify'),
                                        _keyData: keypair.publicKey
                                    }
                                };
                            }
                            throw new Error('Unsupported curve: ' + curve);
                        }

                        if (algoName === 'Ed25519') {
                            const keypair = _crypto.ed25519KeyPair();
                            return {
                                privateKey: {
                                    type: 'private',
                                    extractable,
                                    algorithm: { name: 'Ed25519' },
                                    usages: ['sign'],
                                    _keyData: keypair.privateKey
                                },
                                publicKey: {
                                    type: 'public',
                                    extractable: true,
                                    algorithm: { name: 'Ed25519' },
                                    usages: ['verify'],
                                    _keyData: keypair.publicKey
                                }
                            };
                        }

                        if (algoName === 'X25519') {
                            const keypair = _crypto.x25519KeyPair();
                            return {
                                privateKey: {
                                    type: 'private',
                                    extractable,
                                    algorithm: { name: 'X25519' },
                                    usages: ['deriveKey', 'deriveBits'],
                                    _keyData: keypair.privateKey
                                },
                                publicKey: {
                                    type: 'public',
                                    extractable: true,
                                    algorithm: { name: 'X25519' },
                                    usages: [],
                                    _keyData: keypair.publicKey
                                }
                            };
                        }

                        if (algoName === 'HMAC') {
                            const hashAlgo = algorithm.hash?.name || algorithm.hash || 'SHA-256';
                            const length = algorithm.length || { 'SHA-1': 160, 'SHA-256': 256, 'SHA-384': 384, 'SHA-512': 512 }[hashAlgo] || 256;
                            const keyBytes = randomBytes(length / 8);
                            return {
                                type: 'secret',
                                extractable,
                                algorithm: { name: 'HMAC', hash: { name: hashAlgo }, length },
                                usages: keyUsages,
                                _keyData: keyBytes.buffer.slice(keyBytes.byteOffset, keyBytes.byteOffset + keyBytes.length)
                            };
                        }

                        throw new Error('Unsupported algorithm: ' + algoName);
                    }

                    async importKey(format, keyData, algorithm, extractable, keyUsages) {
                        const algoName = algorithm.name || algorithm;

                        if (format === 'raw') {
                            const data = keyData instanceof ArrayBuffer ? keyData :
                                        ArrayBuffer.isView(keyData) ? keyData.buffer.slice(keyData.byteOffset, keyData.byteOffset + keyData.byteLength) :
                                        new Uint8Array(keyData).buffer;
                            return {
                                type: 'secret',
                                extractable,
                                algorithm: typeof algorithm === 'string' ? { name: algorithm } : algorithm,
                                usages: keyUsages,
                                _keyData: data
                            };
                        }

                        if (format === 'jwk') {
                            // Import from JWK format
                            if (keyData.kty === 'oct') {
                                // Symmetric key
                                const data = Buffer.from(keyData.k, 'base64url');
                                return {
                                    type: 'secret',
                                    extractable,
                                    algorithm: typeof algorithm === 'string' ? { name: algorithm } : algorithm,
                                    usages: keyUsages,
                                    _keyData: data.buffer.slice(data.byteOffset, data.byteOffset + data.length)
                                };
                            }
                            if (keyData.kty === 'EC') {
                                // EC key
                                if (keyData.d) {
                                    // Private key
                                    const d = Buffer.from(keyData.d, 'base64url');
                                    return {
                                        type: 'private',
                                        extractable,
                                        algorithm: typeof algorithm === 'string' ? { name: algorithm } : algorithm,
                                        usages: keyUsages,
                                        _keyData: d.buffer.slice(d.byteOffset, d.byteOffset + d.length)
                                    };
                                } else {
                                    // Public key - reconstruct uncompressed point
                                    const x = Buffer.from(keyData.x, 'base64url');
                                    const y = Buffer.from(keyData.y, 'base64url');
                                    const pubKey = Buffer.alloc(65);
                                    pubKey[0] = 0x04; // Uncompressed point
                                    x.copy(pubKey, 1);
                                    y.copy(pubKey, 33);
                                    return {
                                        type: 'public',
                                        extractable,
                                        algorithm: typeof algorithm === 'string' ? { name: algorithm } : algorithm,
                                        usages: keyUsages,
                                        _keyData: pubKey.buffer.slice(pubKey.byteOffset, pubKey.byteOffset + pubKey.length)
                                    };
                                }
                            }
                            if (keyData.kty === 'OKP') {
                                // Ed25519/X25519 key
                                if (keyData.d) {
                                    const d = Buffer.from(keyData.d, 'base64url');
                                    return {
                                        type: 'private',
                                        extractable,
                                        algorithm: { name: keyData.crv },
                                        usages: keyUsages,
                                        _keyData: d.buffer.slice(d.byteOffset, d.byteOffset + d.length)
                                    };
                                } else {
                                    const x = Buffer.from(keyData.x, 'base64url');
                                    return {
                                        type: 'public',
                                        extractable,
                                        algorithm: { name: keyData.crv },
                                        usages: keyUsages,
                                        _keyData: x.buffer.slice(x.byteOffset, x.byteOffset + x.length)
                                    };
                                }
                            }
                        }

                        throw new Error('Unsupported key format: ' + format);
                    }

                    async exportKey(format, key) {
                        if (!key.extractable) {
                            throw new Error('Key is not extractable');
                        }

                        if (format === 'raw') {
                            return key._keyData;
                        }

                        if (format === 'jwk') {
                            const data = new Uint8Array(key._keyData);
                            const algoName = key.algorithm?.name || '';

                            if (key.type === 'secret') {
                                return {
                                    kty: 'oct',
                                    k: Buffer.from(data).toString('base64url'),
                                    alg: algoName === 'AES-GCM' ? `A${data.length * 8}GCM` :
                                         algoName === 'AES-CBC' ? `A${data.length * 8}CBC` :
                                         algoName === 'HMAC' ? `HS${data.length * 8}` : 'A256GCM',
                                    ext: true,
                                    key_ops: key.usages
                                };
                            }

                            if (algoName === 'Ed25519' || algoName === 'X25519') {
                                if (key.type === 'private') {
                                    return {
                                        kty: 'OKP',
                                        crv: algoName,
                                        d: Buffer.from(data).toString('base64url'),
                                        ext: true,
                                        key_ops: key.usages
                                    };
                                } else {
                                    return {
                                        kty: 'OKP',
                                        crv: algoName,
                                        x: Buffer.from(data).toString('base64url'),
                                        ext: true,
                                        key_ops: key.usages
                                    };
                                }
                            }

                            if (algoName === 'ECDSA' || algoName === 'ECDH') {
                                const curve = key.algorithm?.namedCurve || 'P-256';
                                if (key.type === 'private') {
                                    return {
                                        kty: 'EC',
                                        crv: curve,
                                        d: Buffer.from(data).toString('base64url'),
                                        ext: true,
                                        key_ops: key.usages
                                    };
                                } else {
                                    // Public key is uncompressed point: 0x04 || x || y
                                    if (data[0] === 0x04 && data.length === 65) {
                                        return {
                                            kty: 'EC',
                                            crv: curve,
                                            x: Buffer.from(data.slice(1, 33)).toString('base64url'),
                                            y: Buffer.from(data.slice(33, 65)).toString('base64url'),
                                            ext: true,
                                            key_ops: key.usages
                                        };
                                    }
                                }
                            }
                        }

                        throw new Error('Unsupported export format: ' + format);
                    }

                    async deriveBits(algorithm, baseKey, length) {
                        const algoName = algorithm.name || algorithm;

                        if (algoName === 'X25519' || algoName === 'ECDH') {
                            const privateKey = baseKey._keyData;
                            const publicKey = algorithm.public._keyData;

                            const privBuf = privateKey instanceof ArrayBuffer ? new Uint8Array(privateKey) : privateKey;
                            const pubBuf = publicKey instanceof ArrayBuffer ? new Uint8Array(publicKey) : publicKey;

                            if (algoName === 'X25519') {
                                const shared = _crypto.x25519DeriveSecret(
                                    privBuf.buffer.slice(privBuf.byteOffset, privBuf.byteOffset + privBuf.byteLength),
                                    pubBuf.buffer.slice(pubBuf.byteOffset, pubBuf.byteOffset + pubBuf.byteLength)
                                );
                                return shared.slice(0, length / 8);
                            }

                            if (algorithm.namedCurve === 'P-256' || baseKey.algorithm?.namedCurve === 'P-256') {
                                const shared = _crypto.p256DeriveSecret(
                                    privBuf.buffer.slice(privBuf.byteOffset, privBuf.byteOffset + privBuf.byteLength),
                                    pubBuf.buffer.slice(pubBuf.byteOffset, pubBuf.byteOffset + pubBuf.byteLength)
                                );
                                return shared.slice(0, length / 8);
                            }
                        }

                        if (algoName === 'HKDF') {
                            const salt = algorithm.salt ? new Uint8Array(algorithm.salt) : new Uint8Array(0);
                            const info = algorithm.info ? new Uint8Array(algorithm.info) : new Uint8Array(0);
                            const keyData = new Uint8Array(baseKey._keyData);

                            const result = _modules.crypto.hkdfSync(
                                algorithm.hash?.name || 'SHA-256',
                                Buffer.from(keyData),
                                Buffer.from(salt),
                                Buffer.from(info),
                                length / 8
                            );
                            return result.buffer.slice(result.byteOffset, result.byteOffset + result.length);
                        }

                        if (algoName === 'PBKDF2') {
                            const salt = new Uint8Array(algorithm.salt);
                            const keyData = new Uint8Array(baseKey._keyData);

                            const result = _modules.crypto.pbkdf2Sync(
                                Buffer.from(keyData),
                                Buffer.from(salt),
                                algorithm.iterations,
                                length / 8,
                                algorithm.hash?.name?.replace('-', '').toLowerCase() || 'sha256'
                            );
                            return result.buffer.slice(result.byteOffset, result.byteOffset + result.length);
                        }

                        throw new Error('Unsupported algorithm: ' + algoName);
                    }

                    async deriveKey(algorithm, baseKey, derivedKeyAlgorithm, extractable, keyUsages) {
                        const length = derivedKeyAlgorithm.length || 256;
                        const bits = await this.deriveBits(algorithm, baseKey, length);
                        return {
                            type: 'secret',
                            extractable,
                            algorithm: derivedKeyAlgorithm,
                            usages: keyUsages,
                            _keyData: bits
                        };
                    }

                    async wrapKey(format, key, wrappingKey, wrapAlgorithm) {
                        const exported = await this.exportKey(format, key);
                        const data = format === 'raw' ? exported : new TextEncoder().encode(JSON.stringify(exported));
                        return this.encrypt(wrapAlgorithm, wrappingKey, data);
                    }

                    async unwrapKey(format, wrappedKey, unwrappingKey, unwrapAlgorithm, unwrappedKeyAlgorithm, extractable, keyUsages) {
                        const decrypted = await this.decrypt(unwrapAlgorithm, unwrappingKey, wrappedKey);
                        if (format === 'raw') {
                            return this.importKey('raw', decrypted, unwrappedKeyAlgorithm, extractable, keyUsages);
                        }
                        const jwk = JSON.parse(new TextDecoder().decode(decrypted));
                        return this.importKey('jwk', jwk, unwrappedKeyAlgorithm, extractable, keyUsages);
                    }
                }

                // Create webcrypto object
                const subtle = new SubtleCrypto();
                return {
                    subtle,
                    getRandomValues: function(typedArray) {
                        const bytes = randomBytes(typedArray.byteLength);
                        new Uint8Array(typedArray.buffer, typedArray.byteOffset, typedArray.byteLength).set(bytes);
                        return typedArray;
                    },
                    randomUUID: randomUUID || function() {
                        const bytes = randomBytes(16);
                        bytes[6] = (bytes[6] & 0x0f) | 0x40;
                        bytes[8] = (bytes[8] & 0x3f) | 0x80;
                        const hex = Array.from(bytes).map(b => b.toString(16).padStart(2, '0')).join('');
                        return `${hex.slice(0, 8)}-${hex.slice(8, 12)}-${hex.slice(12, 16)}-${hex.slice(16, 20)}-${hex.slice(20)}`;
                    }
                };
            })()
        };
    }
    _modules['node:crypto'] = _modules.crypto;

