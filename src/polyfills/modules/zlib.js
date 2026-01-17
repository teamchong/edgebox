    // ===== ZLIB MODULE (lazy loaded) =====
    _lazyModule('zlib', function() {
        var zlibModule = {
            // Sync compression functions - use native bindings
            gzipSync: function(buf, options) {
                const input = Buffer.isBuffer(buf) ? buf : Buffer.from(buf);
                if (typeof globalThis.__edgebox_gzip === 'function') {
                    const binStr = String.fromCharCode.apply(null, input);
                    const result = globalThis.__edgebox_gzip(binStr);
                    const bytes = new Uint8Array(result.length);
                    for (let i = 0; i < result.length; i++) bytes[i] = result.charCodeAt(i);
                    return Buffer.from(bytes);
                }
                throw new Error('Native gzip not available');
            },
            gunzipSync: function(buf, options) {
                const input = Buffer.isBuffer(buf) ? buf : Buffer.from(buf);
                if (typeof globalThis.__edgebox_gunzip === 'function') {
                    const binStr = String.fromCharCode.apply(null, input);
                    const result = globalThis.__edgebox_gunzip(binStr);
                    const bytes = new Uint8Array(result.length);
                    for (let i = 0; i < result.length; i++) bytes[i] = result.charCodeAt(i);
                    return Buffer.from(bytes);
                }
                throw new Error('Native gunzip not available');
            },
            deflateSync: function(buf, options) {
                const input = Buffer.isBuffer(buf) ? buf : Buffer.from(buf);
                if (typeof globalThis.__edgebox_deflate === 'function') {
                    const binStr = String.fromCharCode.apply(null, input);
                    const result = globalThis.__edgebox_deflate(binStr);
                    const bytes = new Uint8Array(result.length);
                    for (let i = 0; i < result.length; i++) bytes[i] = result.charCodeAt(i);
                    return Buffer.from(bytes);
                }
                throw new Error('Native deflate not available');
            },
            inflateSync: function(buf, options) {
                const input = Buffer.isBuffer(buf) ? buf : Buffer.from(buf);
                if (typeof globalThis.__edgebox_inflate === 'function') {
                    const binStr = String.fromCharCode.apply(null, input);
                    const result = globalThis.__edgebox_inflate(binStr);
                    const bytes = new Uint8Array(result.length);
                    for (let i = 0; i < result.length; i++) bytes[i] = result.charCodeAt(i);
                    return Buffer.from(bytes);
                }
                throw new Error('Native inflate not available');
            },
            deflateRawSync: function(buf, options) {
                return this.deflateSync(buf, options);
            },
            inflateRawSync: function(buf, options) {
                return this.inflateSync(buf, options);
            },
            brotliCompressSync: function(buf, options) {
                // Brotli not implemented, pass-through
                return Buffer.isBuffer(buf) ? buf : Buffer.from(buf);
            },
            brotliDecompressSync: function(buf, options) {
                return Buffer.isBuffer(buf) ? buf : Buffer.from(buf);
            },

            // Async compression functions
            gzip: function(buf, options, callback) {
                if (typeof options === 'function') { callback = options; options = {}; }
                try {
                    const result = this.gzipSync(buf, options);
                    if (callback) setTimeout(() => callback(null, result), 0);
                    return undefined;
                } catch (e) {
                    if (callback) setTimeout(() => callback(e), 0);
                }
            },
            gunzip: function(buf, options, callback) {
                if (typeof options === 'function') { callback = options; options = {}; }
                try {
                    const result = this.gunzipSync(buf, options);
                    if (callback) setTimeout(() => callback(null, result), 0);
                } catch (e) {
                    if (callback) setTimeout(() => callback(e), 0);
                }
            },
            deflate: function(buf, options, callback) {
                if (typeof options === 'function') { callback = options; options = {}; }
                try {
                    const result = this.deflateSync(buf, options);
                    if (callback) setTimeout(() => callback(null, result), 0);
                } catch (e) {
                    if (callback) setTimeout(() => callback(e), 0);
                }
            },
            inflate: function(buf, options, callback) {
                if (typeof options === 'function') { callback = options; options = {}; }
                try {
                    const result = this.inflateSync(buf, options);
                    if (callback) setTimeout(() => callback(null, result), 0);
                } catch (e) {
                    if (callback) setTimeout(() => callback(e), 0);
                }
            },
            unzip: function(buf, options, callback) {
                // Auto-detect format and decompress
                if (typeof options === 'function') { callback = options; options = {}; }
                try {
                    const input = Buffer.isBuffer(buf) ? buf : Buffer.from(buf);
                    // Check for gzip magic number (1f 8b)
                    if (input.length >= 2 && input[0] === 0x1f && input[1] === 0x8b) {
                        this.gunzip(buf, options, callback);
                    } else {
                        this.inflate(buf, options, callback);
                    }
                } catch (e) {
                    if (callback) setTimeout(() => callback(e), 0);
                }
            },
            brotliCompress: function(buf, options, callback) {
                if (typeof options === 'function') { callback = options; options = {}; }
                try {
                    const result = this.brotliCompressSync(buf, options);
                    if (callback) setTimeout(() => callback(null, result), 0);
                } catch (e) {
                    if (callback) setTimeout(() => callback(e), 0);
                }
            },
            brotliDecompress: function(buf, options, callback) {
                if (typeof options === 'function') { callback = options; options = {}; }
                try {
                    const result = this.brotliDecompressSync(buf, options);
                    if (callback) setTimeout(() => callback(null, result), 0);
                } catch (e) {
                    if (callback) setTimeout(() => callback(e), 0);
                }
            },

            // Stream creators - use zlibModule to avoid circular reference
            createGzip: function(options) {
                const transform = new Transform();
                transform._transform = (chunk, encoding, callback) => {
                    try {
                        callback(null, zlibModule.gzipSync(chunk));
                    } catch (e) {
                        callback(e);
                    }
                };
                return transform;
            },
            createGunzip: function(options) {
                const transform = new Transform();
                transform._transform = (chunk, encoding, callback) => {
                    try {
                        callback(null, zlibModule.gunzipSync(chunk));
                    } catch (e) {
                        callback(e);
                    }
                };
                return transform;
            },
            createDeflate: function(options) {
                const transform = new Transform();
                transform._transform = (chunk, encoding, callback) => {
                    try {
                        callback(null, zlibModule.deflateSync(chunk));
                    } catch (e) {
                        callback(e);
                    }
                };
                return transform;
            },
            createInflate: function(options) {
                const transform = new Transform();
                transform._transform = (chunk, encoding, callback) => {
                    try {
                        callback(null, zlibModule.inflateSync(chunk));
                    } catch (e) {
                        callback(e);
                    }
                };
                return transform;
            },
            createDeflateRaw: function(options) { return this.createDeflate(options); },
            createInflateRaw: function(options) { return this.createInflate(options); },
            createUnzip: function(options) {
                const transform = new Transform();
                const chunks = [];
                transform._transform = (chunk, encoding, callback) => {
                    chunks.push(chunk);
                    callback();
                };
                transform._flush = (callback) => {
                    const buf = Buffer.concat(chunks);
                    // Check for gzip magic number
                    if (buf.length >= 2 && buf[0] === 0x1f && buf[1] === 0x8b) {
                        callback(null, zlibModule.gunzipSync(buf));
                    } else {
                        callback(null, zlibModule.inflateSync(buf));
                    }
                };
                return transform;
            },
            createBrotliCompress: function(options) {
                const transform = new Transform();
                transform._transform = (chunk, encoding, callback) => {
                    callback(null, chunk); // Pass-through for now
                };
                return transform;
            },
            createBrotliDecompress: function(options) {
                const transform = new Transform();
                transform._transform = (chunk, encoding, callback) => {
                    callback(null, chunk); // Pass-through for now
                };
                return transform;
            },

            // Constants
            constants: {
                Z_NO_FLUSH: 0,
                Z_PARTIAL_FLUSH: 1,
                Z_SYNC_FLUSH: 2,
                Z_FULL_FLUSH: 3,
                Z_FINISH: 4,
                Z_BLOCK: 5,
                Z_TREES: 6,
                Z_OK: 0,
                Z_STREAM_END: 1,
                Z_NEED_DICT: 2,
                Z_ERRNO: -1,
                Z_STREAM_ERROR: -2,
                Z_DATA_ERROR: -3,
                Z_MEM_ERROR: -4,
                Z_BUF_ERROR: -5,
                Z_VERSION_ERROR: -6,
                Z_NO_COMPRESSION: 0,
                Z_BEST_SPEED: 1,
                Z_BEST_COMPRESSION: 9,
                Z_DEFAULT_COMPRESSION: -1,
                Z_FILTERED: 1,
                Z_HUFFMAN_ONLY: 2,
                Z_RLE: 3,
                Z_FIXED: 4,
                Z_DEFAULT_STRATEGY: 0,
                BROTLI_OPERATION_PROCESS: 0,
                BROTLI_OPERATION_FLUSH: 1,
                BROTLI_OPERATION_FINISH: 2,
                BROTLI_OPERATION_EMIT_METADATA: 3,
                BROTLI_PARAM_MODE: 0,
                BROTLI_MODE_GENERIC: 0,
                BROTLI_MODE_TEXT: 1,
                BROTLI_MODE_FONT: 2,
                BROTLI_PARAM_QUALITY: 1,
                BROTLI_MIN_QUALITY: 0,
                BROTLI_MAX_QUALITY: 11,
                BROTLI_DEFAULT_QUALITY: 11,
                BROTLI_PARAM_LGWIN: 2,
                BROTLI_MIN_WINDOW_BITS: 10,
                BROTLI_MAX_WINDOW_BITS: 24,
                BROTLI_DEFAULT_WINDOW: 22
            },

            // Zlib class (for advanced usage)
            Zlib: class Zlib extends Transform {
                constructor(mode, options) {
                    super(options);
                    this._mode = mode;
                }
            },
            Gzip: class Gzip extends Transform {},
            Gunzip: class Gunzip extends Transform {},
            Deflate: class Deflate extends Transform {},
            Inflate: class Inflate extends Transform {},
            DeflateRaw: class DeflateRaw extends Transform {},
            InflateRaw: class InflateRaw extends Transform {},
            Unzip: class Unzip extends Transform {},
            BrotliCompress: class BrotliCompress extends Transform {},
            BrotliDecompress: class BrotliDecompress extends Transform {}
        };
        return zlibModule;
    });

    // Module stub
    _modules.module = {
        createRequire: () => globalThis.require,
        Module: { _extensions: {}, _cache: {} }
    };

    // TTY module - provides terminal/TTY functionality
    class TTYReadStream extends Readable {
        constructor(fd) {
            super();
            this.fd = fd;
            this.isTTY = true;
            this.isRaw = false;
            this.setRawMode = (mode) => { this.isRaw = mode; return this; };
        }
    }

    class TTYWriteStream extends Writable {
        constructor(fd) {
            super();
            this.fd = fd;
            this.isTTY = true;
            this._columns = 80;
            this._rows = 24;
            // Use environment variables if available, default to 80x24
            if (globalThis.process && globalThis.process.env) {
                this._columns = parseInt(globalThis.process.env.COLUMNS, 10) || 80;
                this._rows = parseInt(globalThis.process.env.LINES, 10) || 24;
            }
        }
        get columns() { return this._columns; }
        get rows() { return this._rows; }
        getWindowSize() { return [this._columns, this._rows]; }
        // cursorTo - move cursor to absolute position
        cursorTo(x, y) {
            if (y !== undefined) {
                this.write(`\x1b[${y + 1};${x + 1}H`);
            } else {
                this.write(`\x1b[${x + 1}G`);
            }
        }
        // moveCursor - move cursor relative to current position
        moveCursor(dx, dy) {
            if (dx > 0) this.write(`\x1b[${dx}C`);
            else if (dx < 0) this.write(`\x1b[${-dx}D`);
            if (dy > 0) this.write(`\x1b[${dy}B`);
            else if (dy < 0) this.write(`\x1b[${-dy}A`);
        }
        // clearLine - clear current line (-1=left, 0=whole, 1=right)
        clearLine(dir = 0) {
            const code = dir === -1 ? 1 : dir === 1 ? 0 : 2;
            this.write(`\x1b[${code}K`);
        }
        // clearScreenDown - clear from cursor to end of screen
        clearScreenDown() { this.write('\x1b[J'); }
        // getColorDepth - returns color depth supported by terminal
        getColorDepth() { return 8; } // 256 colors
        // hasColors - check if terminal supports N colors
        hasColors(count = 16) { return count <= 256; }
        _write(chunk, encoding, callback) {
            if (typeof __edgebox_stdout_write === 'function') {
                __edgebox_stdout_write(chunk.toString());
            } else {
                print(chunk.toString());
            }
            callback();
        }
    }

    _modules.tty = {
        isatty: fd => fd >= 0 && fd <= 2,
        ReadStream: TTYReadStream,
        WriteStream: TTYWriteStream
    };

    // Process module (alias to globalThis.process)
    _modules.process = globalThis.process;

    // Assert stub
    _modules.assert = function(value, message) { if (!value) throw new Error(message || 'Assertion failed'); };
    _modules.assert.ok = _modules.assert;
    _modules.assert.equal = (a, b, msg) => { if (a != b) throw new Error(msg || `${a} != ${b}`); };
    _modules.assert.strictEqual = (a, b, msg) => { if (a !== b) throw new Error(msg || `${a} !== ${b}`); };
    _modules.assert.deepEqual = (a, b, msg) => { if (JSON.stringify(a) !== JSON.stringify(b)) throw new Error(msg || 'Not deep equal'); };
    _modules.assert.deepStrictEqual = _modules.assert.deepEqual;
    _modules.assert.notEqual = (a, b, msg) => { if (a == b) throw new Error(msg || `${a} == ${b}`); };
    _modules.assert.throws = (fn, msg) => { try { fn(); throw new Error(msg || 'Expected function to throw'); } catch(e) {} };
    _modules.assert.doesNotThrow = (fn, msg) => { try { fn(); } catch(e) { throw new Error(msg || `Function threw: ${e.message}`); } };

    // Buffer module - already set above with native/JS Buffer guard
    // Don't duplicate: _modules.buffer is already set at line 184/188

    // Async hooks module - for async context tracking
    class AsyncLocalStorage {
        constructor() {
            this._store = undefined;
            this._enabled = false;
        }
        disable() { this._enabled = false; }
        getStore() { return this._enabled ? this._store : undefined; }
        run(store, callback, ...args) {
            const prevStore = this._store;
            const prevEnabled = this._enabled;
            this._store = store;
            this._enabled = true;
            try {
                return callback(...args);
            } finally {
                this._store = prevStore;
                this._enabled = prevEnabled;
            }
        }
        exit(callback, ...args) {
            const prevStore = this._store;
            const prevEnabled = this._enabled;
            this._store = undefined;
            this._enabled = false;
            try {
                return callback(...args);
            } finally {
                this._store = prevStore;
                this._enabled = prevEnabled;
            }
        }
        enterWith(store) {
            this._store = store;
            this._enabled = true;
        }
    }

    // Global async context tracking
    const _asyncContext = {
        currentAsyncId: 1,
        currentTriggerAsyncId: 0,
        currentResource: null,
        hooks: [],
        resources: new Map()
    };

    class AsyncResource {
        constructor(type, options = {}) {
            this.type = type;
            this._asyncId = ++AsyncResource._idCounter;
            this._triggerAsyncId = options.triggerAsyncId !== undefined
                ? options.triggerAsyncId
                : _asyncContext.currentAsyncId;
            this._destroyed = false;

            // Track this resource
            _asyncContext.resources.set(this._asyncId, this);

            // Call init hooks
            if (options.requireManualDestroy !== true) {
                _asyncContext.hooks.forEach(hook => {
                    if (hook.enabled && hook.callbacks.init) {
                        try { hook.callbacks.init(this._asyncId, type, this._triggerAsyncId, this); }
                        catch (e) { /* ignore hook errors */ }
                    }
                });
            }
        }

        static _idCounter = 0;

        runInAsyncScope(fn, thisArg, ...args) {
            const prevAsyncId = _asyncContext.currentAsyncId;
            const prevTriggerAsyncId = _asyncContext.currentTriggerAsyncId;
            const prevResource = _asyncContext.currentResource;

            _asyncContext.currentAsyncId = this._asyncId;
            _asyncContext.currentTriggerAsyncId = this._triggerAsyncId;
            _asyncContext.currentResource = this;

            // Call before hooks
            _asyncContext.hooks.forEach(hook => {
                if (hook.enabled && hook.callbacks.before) {
                    try { hook.callbacks.before(this._asyncId); }
                    catch (e) { /* ignore */ }
                }
            });

            try {
                return fn.apply(thisArg, args);
            } finally {
                // Call after hooks
                _asyncContext.hooks.forEach(hook => {
                    if (hook.enabled && hook.callbacks.after) {
                        try { hook.callbacks.after(this._asyncId); }
                        catch (e) { /* ignore */ }
                    }
                });

                _asyncContext.currentAsyncId = prevAsyncId;
                _asyncContext.currentTriggerAsyncId = prevTriggerAsyncId;
                _asyncContext.currentResource = prevResource;
            }
        }

        emitDestroy() {
            if (!this._destroyed) {
                this._destroyed = true;
                _asyncContext.hooks.forEach(hook => {
                    if (hook.enabled && hook.callbacks.destroy) {
                        try { hook.callbacks.destroy(this._asyncId); }
                        catch (e) { /* ignore */ }
                    }
                });
                _asyncContext.resources.delete(this._asyncId);
            }
            return this;
        }

        asyncId() { return this._asyncId; }
        triggerAsyncId() { return this._triggerAsyncId; }

        bind(fn, thisArg) {
            const resource = this;
            const bound = function(...args) {
                return resource.runInAsyncScope(fn, thisArg || this, ...args);
            };
            Object.defineProperty(bound, 'length', { value: fn.length });
            return bound;
        }

        static bind(fn, type, thisArg) {
            const resource = new AsyncResource(type || 'bound-anonymous-fn');
            return resource.bind(fn, thisArg);
        }
    }

    _modules.async_hooks = {
        createHook: function(callbacks) {
            const hook = {
                enabled: false,
                callbacks: callbacks || {},
                enable: function() {
                    this.enabled = true;
                    return this;
                },
                disable: function() {
                    this.enabled = false;
                    return this;
                }
            };
            _asyncContext.hooks.push(hook);
            return hook;
        },
        executionAsyncId: () => _asyncContext.currentAsyncId,
        triggerAsyncId: () => _asyncContext.currentTriggerAsyncId,
        executionAsyncResource: () => _asyncContext.currentResource,
        AsyncLocalStorage,
        AsyncResource
    };
    _modules['node:async_hooks'] = _modules.async_hooks;

    // Timers/promises stub
    _modules['timers/promises'] = {
        setTimeout: (delay) => new Promise(resolve => setTimeout(resolve, delay)),
        setImmediate: () => new Promise(resolve => setImmediate(resolve))
    };

    // Shell quoting helper - escapes arguments for safe shell execution
    // Wraps in single quotes and escapes any embedded single quotes
    function shellQuote(arg) {
        if (arg === '') return "''";
        // If arg contains no special chars, return as-is for readability
        // NOTE: '=' is NOT in whitelist to prevent env var injection (e.g., HOME=/tmp)
        // NOTE: ':' is NOT in whitelist to prevent shell variable expansion
        if (/^[a-zA-Z0-9._\-\/@]+$/.test(arg)) return arg;
        // Wrap in single quotes and escape any embedded single quotes
        // 'foo'bar' -> 'foo'"'"'bar'  (end quote, escaped quote, start quote)
        return "'" + arg.replace(/'/g, "'\"'\"'") + "'";
    }

    // Child process module
    _log('[child_process] Setting up child_process module');
    _modules.child_process = {
        spawnSync: function(cmd, args = [], options = {}) {
            _log('[spawnSync] cmd=' + cmd + ' args=' + JSON.stringify(args));
            // Use native binding if available (__edgebox_spawn returns { status, stdout, stderr })
            if (typeof __edgebox_spawn === 'function') {
                try {
                    // Pass args as array (not JSON string) so nativeSpawn can iterate it
                    const result = __edgebox_spawn(cmd, args || [], JSON.stringify(options.env || {}), options.cwd || null);
                    // Result can be an object directly or a JSON string
                    // exitCode (Component Model path) or status (legacy path)
                    const parsed = (typeof result === 'object' && result !== null) ? result :
                                   (typeof result === 'string' ? JSON.parse(result) : { status: 1 });
                    return {
                        status: parsed.exitCode !== undefined ? parsed.exitCode : (parsed.status || 0),
                        signal: null,
                        stdout: Buffer.from(parsed.stdout || ''),
                        stderr: Buffer.from(parsed.stderr || ''),
                        pid: 0,
                        output: [null, Buffer.from(parsed.stdout || ''), Buffer.from(parsed.stderr || '')]
                    };
                } catch (e) {
                    return {
                        status: 1,
                        signal: null,
                        error: e,
                        stdout: Buffer.from(''),
                        stderr: Buffer.from(e.message || 'spawn failed'),
                        pid: 0,
                        output: [null, Buffer.from(''), Buffer.from(e.message || 'spawn failed')]
                    };
                }
            }
            return { status: 1, error: new Error('child_process not available'), stdout: Buffer.from(''), stderr: Buffer.from('child_process.spawnSync not available in this environment') };
        },
        execSync: function(cmd, options = {}) {
            const result = this.spawnSync('/bin/sh', ['-c', cmd], options);
            if (result.status !== 0) throw Object.assign(new Error(result.stderr?.toString() || 'Command failed'), result);
            return result.stdout;
        },
        execFileSync: function(file, args = [], options = {}) {
            const result = this.spawnSync(file, args, options);
            if (result.status !== 0) throw Object.assign(new Error(result.stderr?.toString() || 'Command failed'), result);
            return result.stdout;
        },
        // Async spawn - returns a ChildProcess-like EventEmitter
        spawn: function(cmd, args = [], options = {}) {
            _log('[spawn ENTER] cmd=' + cmd + ' args=' + JSON.stringify(args) + ' shell=' + options.shell);
            const self = this;
            const child = new EventEmitter();
            child.pid = 1;
            child.killed = false;
            child.exitCode = null;
            child.signalCode = null;
            child.connected = false;
            child.stdin = new Writable();
            child.stdout = new Readable();
            child.stderr = new Readable();
            child.stdio = [child.stdin, child.stdout, child.stderr];
            child.kill = () => { child.killed = true; };
            child.disconnect = () => {};
            child.ref = () => child;
            child.unref = () => child;

            // Use true async spawn if available
            const hasAsyncSpawn = typeof globalThis.__edgebox_spawn_start === 'function';

            if (hasAsyncSpawn) {
                // Build full command with args using proper shell quoting
                // SECURITY: Always quote arguments to prevent command injection
                let fullCmd;
                if (options.shell) {
                    // With shell: true, the command string is passed directly to shell
                    // User intends shell interpretation, so don't escape cmd itself
                    // But args should still be quoted if provided
                    const quotedArgs = args.map(shellQuote).join(' ');
                    const shellCmd = args.length > 0 ? cmd + ' ' + quotedArgs : cmd;
                    fullCmd = shellCmd;
                    _log('[spawn] shell:true cmd=' + cmd.substring(0, 100) + ' fullCmd=' + fullCmd.substring(0, 150));
                } else {
                    // Without shell: true, quote everything to prevent injection
                    // The host will run this through /bin/sh -c anyway
                    const quotedCmd = shellQuote(cmd);
                    const quotedArgs = args.map(shellQuote).join(' ');
                    fullCmd = args.length > 0 ? quotedCmd + ' ' + quotedArgs : quotedCmd;
                    _log('[spawn] shell:false cmd=' + cmd.substring(0, 100));
                }

                // Start async spawn
                const spawnId = globalThis.__edgebox_spawn_start(fullCmd);
                if (spawnId < 0) {
                    setTimeout(() => {
                        // Security: Don't leak internal spawn ID in error message
                        child.emit('error', new Error('Failed to spawn process'));
                    }, 0);
                    return child;
                }

                child.pid = spawnId;

                // Poll for completion with setTimeout to yield control
                const pollForCompletion = () => {
                    if (child.killed) {
                        child.emit('close', -1, 'SIGTERM');
                        return;
                    }

                    const status = globalThis.__edgebox_spawn_poll(spawnId);
                    if (status === 1) {
                        // Complete - get output
                        try {
                            _log('[spawn] poll complete for spawnId=' + spawnId);
                            const result = globalThis.__edgebox_spawn_output(spawnId);
                            _log('[spawn] got result: exitCode=' + result.exitCode);
                            child.exitCode = result.exitCode || 0;
                            // Store output on child object for consumers that read directly
                            child._stdout = result.stdout || '';
                            child._stderr = result.stderr || '';
                            _log('[spawn] set _stdout len=' + child._stdout.length + ' _stderr len=' + child._stderr.length);

                            // Mark as completed - SDK usually checks exitCode
                            child._completed = true;
                            _log('[spawn] child completed, emitting events for spawnId=' + spawnId);
                            // Emit events synchronously (avoiding setTimeout to prevent GC issues)
                            // Wrap each in try/catch to prevent crashes from propagating
                            try { if (child.stdout?.emit) child.stdout.emit('end'); } catch(e) { _log('[spawn] stdout.end error: ' + e.message); }
                            try { if (child.stderr?.emit) child.stderr.emit('end'); } catch(e) { _log('[spawn] stderr.end error: ' + e.message); }
                            try { _log('[spawn] emitting close event'); child.emit('close', child.exitCode, null); _log('[spawn] close emitted'); } catch(e) { _log('[spawn] close emit error: ' + e.message); }
                            try { if (child.emit) child.emit('exit', child.exitCode, null); } catch(e) { _log('[spawn] exit emit error: ' + e.message); }
                        } catch (e) {
                            _log('[spawn] ERROR: ' + e.message);
                            if (typeof child?.emit === 'function') {
                                try { child.emit('error', e); } catch(x) {}
                            }
                        }
                    } else if (status < 0) {
                        child.emit('error', new Error('Spawn failed: ' + status));
                    } else {
                        // Still running - poll again after yielding
                        // _log('[spawn] spawnId=' + spawnId + ' still running, scheduling poll');
                        setTimeout(pollForCompletion, 1);
                    }
                };

                // Start polling
                setTimeout(pollForCompletion, 0);
            } else {
                // Fallback: Execute synchronously in next tick
                setTimeout(() => {
                    try {
                        const result = self.spawnSync(cmd, args, options);
                        child.exitCode = result.status;
                        if (result.stdout && result.stdout.length) child.stdout.emit('data', result.stdout);
                        if (result.stderr && result.stderr.length) child.stderr.emit('data', result.stderr);
                        child.stdout.emit('end');
                        child.stderr.emit('end');
                        child.emit('close', result.status, null);
                        child.emit('exit', result.status, null);
                    } catch (e) {
                        child.emit('error', e);
                    }
                }, 0);
            }
            return child;
        },
        // Async exec - spawns a shell command
        exec: function(cmd, options, callback) {
            if (typeof options === 'function') { callback = options; options = {}; }
            options = options || {};
            const child = this.spawn('/bin/sh', ['-c', cmd], options);
            let stdout = '', stderr = '';
            child.stdout.on('data', d => { stdout += d.toString(); });
            child.stderr.on('data', d => { stderr += d.toString(); });
            child.on('close', (code) => {
                if (callback) {
                    if (code !== 0) {
                        const err = new Error('Command failed: ' + cmd);
                        err.code = code;
                        callback(err, stdout, stderr);
                    } else {
                        callback(null, stdout, stderr);
                    }
                }
            });
            child.on('error', (err) => { if (callback) callback(err, '', ''); });
            return child;
        },
        // Async execFile
        execFile: function(file, args, options, callback) {
            if (typeof args === 'function') { callback = args; args = []; options = {}; }
            if (typeof options === 'function') { callback = options; options = {}; }
            args = args || [];
            options = options || {};
            const child = this.spawn(file, args, options);
            let stdout = '', stderr = '';
            child.stdout.on('data', d => { stdout += d.toString(); });
            child.stderr.on('data', d => { stderr += d.toString(); });
            child.on('close', (code) => {
                if (callback) {
                    if (code !== 0) {
                        const err = new Error('Command failed: ' + file);
                        err.code = code;
                        callback(err, stdout, stderr);
                    } else {
                        callback(null, stdout, stderr);
                    }
                }
            });
            child.on('error', (err) => { if (callback) callback(err, '', ''); });
            return child;
        },
        fork: function() { throw new Error('fork not supported in WASI environment'); }
    };

    // Path submodules
    _modules['path/win32'] = _modules.path;
    _modules['path/posix'] = _modules.path;

    // DNS module - limited in WASM, provides basic stubs (LAZY LOADED)
    _lazyModule('dns', function() {
        return {
            lookup: function(hostname, options, callback) {
                if (typeof options === 'function') { callback = options; options = {}; }
                if (hostname === 'localhost' || hostname === '127.0.0.1' || hostname === '::1') {
                    setTimeout(() => callback(null, '127.0.0.1', 4), 0);
                } else {
                    setTimeout(() => callback(null, '127.0.0.1', 4), 0);
                }
            },
            resolve: function(hostname, rrtype, callback) {
                if (typeof rrtype === 'function') { callback = rrtype; rrtype = 'A'; }
                setTimeout(() => callback(null, ['127.0.0.1']), 0);
            },
            resolve4: function(hostname, callback) { this.resolve(hostname, 'A', callback); },
            resolve6: function(hostname, callback) { setTimeout(() => callback(null, ['::1']), 0); },
            resolveCname: function(hostname, callback) { setTimeout(() => callback(null, []), 0); },
            resolveMx: function(hostname, callback) { setTimeout(() => callback(null, []), 0); },
            resolveNs: function(hostname, callback) { setTimeout(() => callback(null, []), 0); },
            resolveTxt: function(hostname, callback) { setTimeout(() => callback(null, []), 0); },
            resolveSrv: function(hostname, callback) { setTimeout(() => callback(null, []), 0); },
            resolvePtr: function(hostname, callback) { setTimeout(() => callback(null, []), 0); },
            resolveNaptr: function(hostname, callback) { setTimeout(() => callback(null, []), 0); },
            resolveSoa: function(hostname, callback) { setTimeout(() => callback(null, null), 0); },
            reverse: function(ip, callback) { setTimeout(() => callback(null, []), 0); },
            setServers: function(servers) { /* no-op in WASM */ },
            getServers: function() { return ['127.0.0.1']; },
            promises: {
                lookup: (hostname, options) => Promise.resolve({ address: '127.0.0.1', family: 4 }),
                resolve: (hostname, rrtype) => Promise.resolve(['127.0.0.1']),
                resolve4: (hostname) => Promise.resolve(['127.0.0.1']),
                resolve6: (hostname) => Promise.resolve(['::1']),
                setServers: (servers) => {},
                getServers: () => ['127.0.0.1']
            },
            ADDRCONFIG: 1024, V4MAPPED: 2048,
            NODATA: 'ENODATA', FORMERR: 'EFORMERR', SERVFAIL: 'ESERVFAIL',
            NOTFOUND: 'ENOTFOUND', NOTIMP: 'ENOTIMP', REFUSED: 'EREFUSED',
            BADQUERY: 'EBADQUERY', BADNAME: 'EBADNAME', BADFAMILY: 'EBADFAMILY',
            BADRESP: 'EBADRESP', CONNREFUSED: 'ECONNREFUSED', TIMEOUT: 'ETIMEOUT',
            EOF: 'EOF', FILE: 'EFILE', NOMEM: 'ENOMEM', DESTRUCTION: 'EDESTRUCTION',
            BADSTR: 'EBADSTR', BADFLAGS: 'EBADFLAGS', NONAME: 'ENONAME',
            BADHINTS: 'EBADHINTS', NOTINITIALIZED: 'ENOTINITIALIZED',
            LOADIPHLPAPI: 'ELOADIPHLPAPI', ADDRGETNETWORKPARAMS: 'EADDRGETNETWORKPARAMS',
            CANCELLED: 'ECANCELLED'
        };
    });

    // Inspector module - debugging not available in WASM (lazy loaded)
    _lazyModule('inspector', function() {
        return {
            open: function(port, host, wait) {
                console.warn('inspector.open() not available in WASM environment');
            },
            close: function() {},
            url: function() { return undefined; },
            waitForDebugger: function() {
                console.warn('inspector.waitForDebugger() not available in WASM environment');
            },
            console: globalThis.console,
            Session: class Session extends EventEmitter {
                constructor() { super(); }
                connect() { console.warn('Inspector sessions not available in WASM'); }
                connectToMainThread() { this.connect(); }
                disconnect() {}
                post(method, params, callback) {
                    if (callback) setTimeout(() => callback(new Error('Inspector not available')), 0);
                }
            }
        };
    });

    // V8 module - V8-specific APIs not available in QuickJS (lazy loaded)
    _lazyModule('v8', function() {
        return {
            cachedDataVersionTag: function() { return 0; },
            getHeapStatistics: function() {
                return {
                    total_heap_size: 0,
                    total_heap_size_executable: 0,
                    total_physical_size: 0,
                    total_available_size: 0,
                    used_heap_size: 0,
                    heap_size_limit: 0,
                    malloced_memory: 0,
                    peak_malloced_memory: 0,
                    does_zap_garbage: 0,
                    number_of_native_contexts: 0,
                    number_of_detached_contexts: 0
                };
            },
            getHeapSpaceStatistics: function() { return []; },
            getHeapSnapshot: function() {
                throw new Error('Heap snapshots not available in QuickJS/WASM environment');
            },
            getHeapCodeStatistics: function() {
                return { code_and_metadata_size: 0, bytecode_and_metadata_size: 0, external_script_source_size: 0 };
            },
            setFlagsFromString: function(flags) {
                console.warn('v8.setFlagsFromString() has no effect in QuickJS environment');
            },
            writeHeapSnapshot: function(filename) {
                throw new Error('Heap snapshots not available in QuickJS/WASM environment');
            },
            serialize: function(value) {
                // Simple serialization using JSON (V8 uses structured clone)
                return Buffer.from(JSON.stringify(value));
            },
            deserialize: function(buffer) {
                return JSON.parse(buffer.toString());
            },
            Serializer: class Serializer {
                constructor() { this._buffer = []; }
                writeHeader() {}
                writeValue(value) { this._buffer.push(JSON.stringify(value)); }
                releaseBuffer() { return Buffer.from(this._buffer.join('')); }
            },
            Deserializer: class Deserializer {
                constructor(buffer) { this._data = buffer.toString(); }
                readHeader() { return true; }
                readValue() { return JSON.parse(this._data); }
            },
            DefaultSerializer: class DefaultSerializer {},
            DefaultDeserializer: class DefaultDeserializer {},
            promiseHooks: {
                onInit: () => {},
                onSettled: () => {},
                onBefore: () => {},
                onAfter: () => {},
                createHook: () => ({ enable: () => {}, disable: () => {} })
            }
        };
    });

    // Readline module - line-by-line input interface
    class ReadlineInterface extends EventEmitter {
        constructor(options = {}) {
            super();
            this.input = options.input || process.stdin;
            this.output = options.output || process.stdout;
            this.terminal = options.terminal !== false;
            this.line = '';
            this.cursor = 0;
            this.history = [];
            this.historyIndex = -1;
            this.closed = false;
            this._prompt = '> ';
        }
        setPrompt(prompt) { this._prompt = prompt; }
        prompt(preserveCursor) {
            if (!preserveCursor) this.cursor = 0;
            this.output.write(this._prompt);
        }
        question(query, callback) {
            this.output.write(query);
            // In non-interactive mode, immediately return empty string
            if (typeof callback === 'function') {
                setTimeout(() => callback(''), 0);
            }
        }
        write(data, key) {
            if (typeof data === 'string') {
                this.line += data;
                this.cursor += data.length;
            }
        }
        close() {
            if (!this.closed) {
                this.closed = true;
                this.emit('close');
            }
        }
        pause() { this.emit('pause'); return this; }
        resume() { this.emit('resume'); return this; }
        // Cursor movement
        getCursorPos() { return { rows: 0, cols: this.cursor }; }
    }

    _modules.readline = {
        createInterface: (options) => new ReadlineInterface(options),
        clearLine: (stream, dir) => { if (stream && stream.clearLine) stream.clearLine(dir); },
        clearScreenDown: (stream) => { if (stream && stream.clearScreenDown) stream.clearScreenDown(); },
        cursorTo: (stream, x, y) => { if (stream && stream.cursorTo) stream.cursorTo(x, y); },
        moveCursor: (stream, dx, dy) => { if (stream && stream.moveCursor) stream.moveCursor(dx, dy); },
        emitKeypressEvents: (stream) => { /* no-op in WASI */ }
    };
    _modules['readline/promises'] = {
        createInterface: (options) => {
            const rl = new ReadlineInterface(options);
            rl.question = (query) => new Promise(resolve => {
                _modules.readline.createInterface(options).question(query, resolve);
            });
            return rl;
        }
    };

    // String decoder stub
    _modules.string_decoder = {
        StringDecoder: class StringDecoder {
            constructor(encoding) { this.encoding = encoding || 'utf8'; }
            write(buf) { return buf.toString(this.encoding); }
            end(buf) { return buf ? buf.toString(this.encoding) : ''; }
        }
    };

    // Querystring module
    // ONLY create JS querystring if native Zig querystring doesn't exist
    // Native querystring is in src/polyfills/querystring.zig
    if (!_modules.querystring) {
        _modules.querystring = {
        parse: str => Object.fromEntries(new URLSearchParams(str)),
        stringify: obj => new URLSearchParams(obj).toString(),
        escape: encodeURIComponent,
        unescape: decodeURIComponent
        };
    }

    // Performance hooks module
    const _perfStart = Date.now();
    _modules.perf_hooks = {
        performance: {
            now: () => Date.now() - _perfStart,
            timeOrigin: _perfStart,
            mark: () => {},
            measure: () => {},
            clearMarks: () => {},
            clearMeasures: () => {},
            getEntries: () => [],
            getEntriesByName: () => [],
            getEntriesByType: () => []
        },
        PerformanceObserver: class PerformanceObserver {
            constructor(callback) { this.callback = callback; }
            observe() {}
            disconnect() {}
        },
        monitorEventLoopDelay: () => ({
            enable: () => {},
            disable: () => {},
            percentile: () => 0,
            min: 0,
            max: 0,
            mean: 0,
            stddev: 0
        })
    };

    // VM module - for running code in V8/QuickJS contexts
    _modules.vm = {
        createContext: (sandbox) => sandbox || {},
        runInContext: (code, context) => {
            const fn = new Function(...Object.keys(context), code);
            return fn(...Object.values(context));
        },
        runInNewContext: (code, context) => _modules.vm.runInContext(code, context || {}),
        runInThisContext: (code) => eval(code),
        Script: class Script {
            constructor(code) { this.code = code; }
            runInContext(context) { return _modules.vm.runInContext(this.code, context); }
            runInNewContext(context) { return this.runInContext(context || {}); }
            runInThisContext() { return eval(this.code); }
        },
        isContext: () => true,
        compileFunction: (code, params = []) => new Function(...params, code)
    };

    // Worker threads module - implemented via subprocess IPC
    // Each worker is a separate WASM instance communicating via JSON messages over pipes
    var _workerIdCounter = 0;
    var _isWorkerProcess = typeof globalThis.__edgebox_worker_id !== 'undefined';
    var _parentPort = null;

    // MessagePort implementation for IPC
    class MessagePort extends EventEmitter {
        constructor() {
            super();
            this._closed = false;
        }
        postMessage(value) {
            if (this._closed) return;
            // Serialize and send via IPC (implemented by Worker/parentPort)
            if (this._sendMessage) {
                this._sendMessage(JSON.stringify({ type: 'message', data: value }));
            }
            this.emit('message', value);
        }
        close() {
            this._closed = true;
            this.emit('close');
        }
        start() {}
        ref() { return this; }
        unref() { return this; }
    }

    class MessageChannel {
        constructor() {
            this.port1 = new MessagePort();
            this.port2 = new MessagePort();
            // Connect ports - messages sent to one appear on the other
            var self = this;
            // Security: Wrap JSON.parse in try/catch to prevent crashes
            this.port1._sendMessage = function(msg) { try { self.port2.emit('message', JSON.parse(msg).data); } catch (e) {} };
            this.port2._sendMessage = function(msg) { try { self.port1.emit('message', JSON.parse(msg).data); } catch (e) {} };
        }
    }

    class Worker extends EventEmitter {
        constructor(filename, options) {
            super();
            options = options || {};
            var self = this;
            this.threadId = ++_workerIdCounter;
            this._exited = false;

            // Check if we have subprocess capability
            if (typeof globalThis.__edgebox_spawn_start !== 'function') {
                throw new Error('Worker threads require async spawn support');
            }

            // Prepare worker data
            var workerDataJson = options.workerData ? JSON.stringify(options.workerData) : 'null';

            // Create a wrapper script that sets up the worker environment
            var workerScript = filename;
            if (options.eval) {
                workerScript = filename; // Code is passed as string
            }

            // Spawn worker process with worker-specific env
            // Use explicit property copying to prevent prototype pollution
            var env = {};
            var procEnvKeys = Object.keys(process.env);
            for (var i = 0; i < procEnvKeys.length; i++) {
                var key = procEnvKeys[i];
                env[key] = process.env[key];
            }
            if (options.env) {
                var optEnvKeys = Object.keys(options.env);
                for (var j = 0; j < optEnvKeys.length; j++) {
                    var optKey = optEnvKeys[j];
                    if (Object.prototype.hasOwnProperty.call(options.env, optKey)) {
                        env[optKey] = options.env[optKey];
                    }
                }
            }
            env.__EDGEBOX_WORKER_ID = String(this.threadId);
            env.__EDGEBOX_WORKER_DATA = workerDataJson;

            // Use child_process.spawn for the worker
            var cp = _modules.child_process;
            this._child = cp.spawn(process.argv[0], [workerScript], {
                env: env,
                stdio: ['pipe', 'pipe', 'pipe']
            });

            this._child.stdout.on('data', function(data) {
                var lines = data.toString().split('\n');
                for (var i = 0; i < lines.length; i++) {
                    var line = lines[i].trim();
                    if (line.startsWith('__WORKER_MSG__:')) {
                        try {
                            var msg = JSON.parse(line.slice(15));
                            if (msg.type === 'message') {
                                self.emit('message', msg.data);
                            }
                        } catch (e) {}
                    } else if (line) {
                        // Regular stdout output
                        console.log('[Worker ' + self.threadId + ']', line);
                    }
                }
            });

            this._child.stderr.on('data', function(data) {
                self.emit('error', new Error(data.toString()));
            });

            this._child.on('exit', function(code) {
                self._exited = true;
                self.emit('exit', code);
            });

            this._child.on('error', function(err) {
                self.emit('error', err);
            });

            // Emit online after a short delay
            setTimeout(function() { self.emit('online'); }, 0);
        }

        postMessage(value) {
            if (this._exited || !this._child) return;
            var msg = '__PARENT_MSG__:' + JSON.stringify({ type: 'message', data: value }) + '\n';
            if (this._child.stdin && this._child.stdin.write) {
                this._child.stdin.write(msg);
            }
        }

        terminate(callback) {
            var self = this;
            if (this._child) {
                this._child.kill();
            }
            this._exited = true;
            if (callback) setTimeout(function() { callback(null, 0); }, 0);
            return Promise.resolve(0);
        }

        ref() { return this; }
        unref() { return this; }
    }

    // Set up parentPort if we're in a worker
    if (_isWorkerProcess) {
        _parentPort = new MessagePort();
        _parentPort._sendMessage = function(msg) {
            print('__WORKER_MSG__:' + msg);
        };

        // Listen for messages from parent on stdin
        if (typeof globalThis.__edgebox_read_stdin === 'function') {
            var stdinBuffer = '';
            setInterval(function() {
                var data = globalThis.__edgebox_read_stdin(4096);
                if (data && data.length > 0) {
                    stdinBuffer += data;
                    var lines = stdinBuffer.split('\n');
                    stdinBuffer = lines.pop(); // Keep incomplete line
                    for (var i = 0; i < lines.length; i++) {
                        var line = lines[i].trim();
                        if (line.startsWith('__PARENT_MSG__:')) {
                            try {
                                var msg = JSON.parse(line.slice(15));
                                if (msg.type === 'message') {
                                    _parentPort.emit('message', msg.data);
                                }
                            } catch (e) {}
                        }
                    }
                }
            }, 10);
        }
    }

