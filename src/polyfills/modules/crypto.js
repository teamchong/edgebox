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
            randomBytes: randomBytes || function(size) {
                throw new Error('crypto.randomBytes not available - Zig native not registered');
            },
            randomUUID: randomUUID || function() {
                throw new Error('crypto.randomUUID not available - Zig native not registered');
            },
            getHashes: () => ['sha256', 'sha384', 'sha512', 'sha1', 'md5'],

            // createHash - wrapper that calls Zig hash on digest()
            createHash: function(algorithm) {
                const algo = algorithm.toLowerCase();
                let data = '';
                return {
                    update(input) {
                        data += typeof input === 'string' ? input : String.fromCharCode.apply(null, input);
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
                    }
                };
            },

            // createHmac - wrapper that calls Zig hmac on digest()
            createHmac: function(algorithm, key) {
                const algo = algorithm.toLowerCase();
                const keyStr = typeof key === 'string' ? key : String.fromCharCode.apply(null, key);
                let data = '';
                return {
                    update(input) {
                        data += typeof input === 'string' ? input : String.fromCharCode.apply(null, input);
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

