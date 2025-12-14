// EdgeBox Thin ABI Wrapper
//
// This is a THIN wrapper (~100 lines) around the Zig ABI.
// NO parsing, NO complex logic - just memory management and calls.
//
// All heavy lifting (HTTP parsing, crypto, etc.) runs in Zig/WASM.
// JS just shuffles bytes across the boundary.

(function() {
    'use strict';

    // Text encoder/decoder for string conversion
    const encoder = new TextEncoder();
    const decoder = new TextDecoder();

    // WASM memory reference (set during initialization)
    let memory = null;

    // ========================================================================
    // Memory Helpers
    // ========================================================================

    function encodeString(str) {
        const bytes = encoder.encode(str);
        const ptr = globalThis.__zig_alloc(bytes.length);
        if (ptr === 0) throw new Error('Allocation failed');
        new Uint8Array(memory.buffer, ptr, bytes.length).set(bytes);
        return { ptr, len: bytes.length };
    }

    function decodeResult(ptr) {
        if (ptr === 0) return null;
        const view = new DataView(memory.buffer);
        const len = view.getUint32(ptr, true); // little-endian
        const bytes = new Uint8Array(memory.buffer, ptr + 4, len);
        return decoder.decode(bytes);
    }

    function decodeResultBytes(ptr) {
        if (ptr === 0) return null;
        const view = new DataView(memory.buffer);
        const len = view.getUint32(ptr, true);
        return new Uint8Array(memory.buffer, ptr + 4, len).slice(); // copy
    }

    function freeResult(ptr) {
        if (ptr !== 0) globalThis.__zig_free_result(ptr);
    }

    // ========================================================================
    // Zig ABI Object
    // ========================================================================

    const zig = {
        // Initialize with WASM memory reference
        _init(wasmMemory) {
            memory = wasmMemory;
        },

        // ====================================================================
        // Crypto
        // ====================================================================

        hash(algorithm, data) {
            const algo = { sha256: 0, sha384: 1, sha512: 2, md5: 3, sha1: 4 }[algorithm];
            if (algo === undefined) throw new Error('Unknown algorithm: ' + algorithm);

            const d = encodeString(typeof data === 'string' ? data : String(data));
            const result = globalThis.__zig_hash(algo, d.ptr, d.len);
            globalThis.__zig_free(d.ptr, d.len);

            const hex = decodeResult(result);
            freeResult(result);
            return hex;
        },

        hmac(algorithm, key, data) {
            const algo = { sha256: 0, sha384: 1, sha512: 2, md5: 3, sha1: 4 }[algorithm];
            if (algo === undefined) throw new Error('Unknown algorithm: ' + algorithm);

            const k = encodeString(typeof key === 'string' ? key : String(key));
            const d = encodeString(typeof data === 'string' ? data : String(data));
            const result = globalThis.__zig_hmac(algo, k.ptr, k.len, d.ptr, d.len);
            globalThis.__zig_free(k.ptr, k.len);
            globalThis.__zig_free(d.ptr, d.len);

            const hex = decodeResult(result);
            freeResult(result);
            return hex;
        },

        randomBytes(size) {
            const result = globalThis.__zig_random_bytes(size);
            const bytes = decodeResultBytes(result);
            freeResult(result);
            return bytes;
        },

        randomUUID() {
            const result = globalThis.__zig_random_uuid();
            const uuid = decodeResult(result);
            freeResult(result);
            return uuid;
        },

        // ====================================================================
        // Filesystem
        // ====================================================================

        readFileSync(path) {
            const p = encodeString(path);
            const result = globalThis.__zig_fs_read(p.ptr, p.len);
            globalThis.__zig_free(p.ptr, p.len);

            if (result === 0) throw new Error('ENOENT: ' + path);
            const content = decodeResult(result);
            freeResult(result);
            return content;
        },

        writeFileSync(path, data) {
            const p = encodeString(path);
            const d = encodeString(typeof data === 'string' ? data : String(data));
            const result = globalThis.__zig_fs_write(p.ptr, p.len, d.ptr, d.len);
            globalThis.__zig_free(p.ptr, p.len);
            globalThis.__zig_free(d.ptr, d.len);

            if (result !== 0) throw new Error('EACCES: ' + path);
        },

        existsSync(path) {
            const p = encodeString(path);
            const result = globalThis.__zig_fs_exists(p.ptr, p.len);
            globalThis.__zig_free(p.ptr, p.len);
            return result === 1;
        },

        statSync(path) {
            const p = encodeString(path);
            const result = globalThis.__zig_fs_stat(p.ptr, p.len);
            globalThis.__zig_free(p.ptr, p.len);

            if (result === 0) throw new Error('ENOENT: ' + path);
            const json = decodeResult(result);
            freeResult(result);
            return JSON.parse(json);
        },

        readdirSync(path) {
            const p = encodeString(path);
            const result = globalThis.__zig_fs_readdir(p.ptr, p.len);
            globalThis.__zig_free(p.ptr, p.len);

            if (result === 0) throw new Error('ENOENT: ' + path);
            const json = decodeResult(result);
            freeResult(result);
            return JSON.parse(json);
        },

        // ====================================================================
        // Process
        // ====================================================================

        spawnSync(command, args, options) {
            args = args || [];
            options = options || {};

            // Build command string (shell-style)
            const cmd = [command, ...args].join(' ');
            const c = encodeString(cmd);
            const timeout = options.timeout || 30000;

            const result = globalThis.__zig_spawn_sync(c.ptr, c.len, timeout);
            globalThis.__zig_free(c.ptr, c.len);

            if (result === 0) throw new Error('Spawn failed: ' + command);
            const json = decodeResult(result);
            freeResult(result);
            return JSON.parse(json);
        },

        // ====================================================================
        // HTTP (placeholder - to be implemented)
        // ====================================================================

        fetch(url, options) {
            options = options || {};
            const method = { GET: 0, POST: 1, PUT: 2, DELETE: 3, PATCH: 4, HEAD: 5 }[options.method || 'GET'];

            const u = encodeString(url);
            const h = options.headers ? encodeString(JSON.stringify(options.headers)) : { ptr: 0, len: 0 };
            const b = options.body ? encodeString(options.body) : { ptr: 0, len: 0 };

            const result = globalThis.__zig_fetch(u.ptr, u.len, method, h.ptr, h.len, b.ptr, b.len);

            globalThis.__zig_free(u.ptr, u.len);
            if (h.ptr) globalThis.__zig_free(h.ptr, h.len);
            if (b.ptr) globalThis.__zig_free(b.ptr, b.len);

            if (result === 0) throw new Error('Fetch failed: ' + url);

            const status = globalThis.__zig_fetch_status(result);
            const body = decodeResult(globalThis.__zig_fetch_body(result));
            globalThis.__zig_fetch_free(result);

            return { status, ok: status >= 200 && status < 300, body };
        },

        // ====================================================================
        // Utility
        // ====================================================================

        base64Encode(data) {
            const d = typeof data === 'string' ? encodeString(data) : { ptr: 0, len: 0 };
            const result = globalThis.__zig_base64_encode(d.ptr, d.len);
            if (d.ptr) globalThis.__zig_free(d.ptr, d.len);

            const encoded = decodeResult(result);
            freeResult(result);
            return encoded;
        },

        base64Decode(data) {
            const d = encodeString(data);
            const result = globalThis.__zig_base64_decode(d.ptr, d.len);
            globalThis.__zig_free(d.ptr, d.len);

            const decoded = decodeResultBytes(result);
            freeResult(result);
            return decoded;
        },

        timeNow() {
            return globalThis.__zig_time_now();
        }
    };

    // ========================================================================
    // Node.js Compatibility Shims
    // ========================================================================

    globalThis.require = function(name) {
        if (name === 'crypto') {
            return {
                createHash(algo) {
                    let data = '';
                    return {
                        update(d) { data += d; return this; },
                        digest(enc) {
                            const hex = zig.hash(algo, data);
                            if (enc === 'hex') return hex;
                            if (enc === 'base64') return btoa(hex); // simplified
                            return hex;
                        }
                    };
                },
                createHmac(algo, key) {
                    let data = '';
                    return {
                        update(d) { data += d; return this; },
                        digest(enc) {
                            const hex = zig.hmac(algo, key, data);
                            if (enc === 'hex') return hex;
                            return hex;
                        }
                    };
                },
                randomBytes(n) { return zig.randomBytes(n); },
                randomUUID() { return zig.randomUUID(); }
            };
        }
        if (name === 'fs') {
            return {
                readFileSync(p, opts) {
                    const content = zig.readFileSync(p);
                    if (opts && opts.encoding === 'utf8') return content;
                    return { toString() { return content; } };
                },
                writeFileSync(p, d) { return zig.writeFileSync(p, d); },
                existsSync(p) { return zig.existsSync(p); },
                statSync(p) { return zig.statSync(p); },
                readdirSync(p) { return zig.readdirSync(p); }
            };
        }
        if (name === 'child_process') {
            return {
                spawnSync(cmd, args, opts) { return zig.spawnSync(cmd, args, opts); }
            };
        }
        if (name === 'path') {
            return {
                join(...parts) { return parts.join('/').replace(/\/+/g, '/'); },
                dirname(p) { return p.split('/').slice(0, -1).join('/') || '.'; },
                basename(p) { return p.split('/').pop(); },
                resolve(...parts) { return parts.join('/').replace(/\/+/g, '/'); }
            };
        }
        throw new Error('Module not found: ' + name);
    };

    // Export
    globalThis.__zig = zig;

})();
