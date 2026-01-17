    // ===== CRYPTO MODULE =====
    // ONLY create JS crypto if native Zig crypto doesn't exist
    // Native crypto is in src/polyfills/crypto.zig with: hash, hmac, aesGcmEncrypt, aesGcmDecrypt
    if (!_modules.crypto) {
        _modules.crypto = {
            randomBytes: function(size) {
                const buf = new Uint8Array(size);
                for (let i = 0; i < size; i++) buf[i] = Math.floor(Math.random() * 256);
                return Buffer.from(buf);
            },
        randomUUID: function() {
            const bytes = this.randomBytes(16);
            bytes[6] = (bytes[6] & 0x0f) | 0x40;
            bytes[8] = (bytes[8] & 0x3f) | 0x80;
            const hex = Array.from(bytes).map(b => b.toString(16).padStart(2, '0')).join('');
            return hex.slice(0,8)+'-'+hex.slice(8,12)+'-'+hex.slice(12,16)+'-'+hex.slice(16,20)+'-'+hex.slice(20);
        },
        // Hash algorithms supported by native code
        getHashes: function() {
            return ['sha256', 'sha384', 'sha512', 'sha1', 'md5'];
        },
        // createHash - returns a Hash object with update() and digest()
        createHash: function(algorithm) {
            const algo = algorithm.toLowerCase();
            let data = '';
            // Security: Max input size limit to prevent DoS (100MB)
            const MAX_INPUT_SIZE = 100 * 1024 * 1024;
            return {
                update: function(input) {
                    // Security: Limit input size to prevent String.fromCharCode DoS
                    if (typeof input !== 'string' && input.length > MAX_INPUT_SIZE) {
                        throw new RangeError('Input too large for hash');
                    }
                    data += typeof input === 'string' ? input : String.fromCharCode.apply(null, input);
                    return this;
                },
                digest: function(encoding) {
                    // Call native __edgebox_hash(algorithm, data)
                    const result = globalThis.__edgebox_hash(algo, data);
                    if (encoding === 'hex') return result;
                    if (encoding === 'base64') {
                        // Convert hex to base64
                        const bytes = [];
                        for (let i = 0; i < result.length; i += 2) {
                            bytes.push(parseInt(result.substring(i, i + 2), 16));
                        }
                        return btoa(String.fromCharCode.apply(null, bytes));
                    }
                    // Return as Buffer by default
                    const bytes = [];
                    for (let i = 0; i < result.length; i += 2) {
                        bytes.push(parseInt(result.substring(i, i + 2), 16));
                    }
                    return Buffer.from(bytes);
                }
            };
        },
        // createHmac - returns an Hmac object with update() and digest()
        createHmac: function(algorithm, key) {
            const algo = algorithm.toLowerCase();
            // Security: Max input size limit to prevent DoS (100MB)
            const MAX_INPUT_SIZE = 100 * 1024 * 1024;
            if (typeof key !== 'string' && key.length > MAX_INPUT_SIZE) {
                throw new RangeError('Key too large for HMAC');
            }
            const keyStr = typeof key === 'string' ? key : String.fromCharCode.apply(null, key);
            let data = '';
            return {
                update: function(input) {
                    // Security: Limit input size
                    if (typeof input !== 'string' && input.length > MAX_INPUT_SIZE) {
                        throw new RangeError('Input too large for HMAC');
                    }
                    data += typeof input === 'string' ? input : String.fromCharCode.apply(null, input);
                    return this;
                },
                digest: function(encoding) {
                    // Call native __edgebox_hmac(algorithm, key, data)
                    const result = globalThis.__edgebox_hmac(algo, keyStr, data);
                    if (encoding === 'hex') return result;
                    if (encoding === 'base64') {
                        const bytes = [];
                        for (let i = 0; i < result.length; i += 2) {
                            bytes.push(parseInt(result.substring(i, i + 2), 16));
                        }
                        return btoa(String.fromCharCode.apply(null, bytes));
                    }
                    const bytes = [];
                    for (let i = 0; i < result.length; i += 2) {
                        bytes.push(parseInt(result.substring(i, i + 2), 16));
                    }
                    return Buffer.from(bytes);
                }
            };
        }
        };
    } else {
        // Native crypto exists - add missing JS-only functions if needed
        if (!_modules.crypto.randomBytes) {
            _modules.crypto.randomBytes = function(size) {
                const buf = new Uint8Array(size);
                for (let i = 0; i < size; i++) buf[i] = Math.floor(Math.random() * 256);
                return Buffer.from(buf);
            };
        }
    }

