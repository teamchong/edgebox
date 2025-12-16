(function() {
    'use strict';
    // Debug flag - disabled for performance in production
    const _debug = false; // globalThis._polyfillDebug || ...
    const _log = _debug ? print : function() {};

    _log('[node_polyfill] START - globalThis._os?.setTimeout: ' + (typeof globalThis._os?.setTimeout));

    // GUARD: Skip polyfill initialization if already done (Wizer pre-initialized)
    if (globalThis._polyfillsInitialized) {
        return;
    }

    // Try to get QuickJS os module - needed for file descriptor operations
    let _os = null;
    try {
        if (typeof globalThis._os !== 'undefined') {
            _os = globalThis._os;
        } else if (typeof os !== 'undefined') {
            _os = os;
            globalThis._os = os;
        }
    } catch(e) {}

    // Module registry - use globalThis._modules for compatibility with require()
    globalThis._modules = globalThis._modules || {};
    const _modules = globalThis._modules;

    // ===== MOUNT PATH REMAPPING (Docker-style volumes) =====
    // Parse __EDGEBOX_MOUNTS env var: [{"host":"/tmp/edgebox-home","guest":"/Users/name"}]
    let _edgeboxMounts = [];
    try {
        if (typeof std !== 'undefined' && std.getenv) {
            const mountsJson = std.getenv('__EDGEBOX_MOUNTS');
            if (mountsJson) {
                _edgeboxMounts = JSON.parse(mountsJson);
                // Sort by guest path length (longest first for specificity)
                _edgeboxMounts.sort((a, b) => b.guest.length - a.guest.length);
                _log('[node_polyfill] Mounts loaded: ' + JSON.stringify(_edgeboxMounts));
            }
        }
    } catch (e) {
        _log('[node_polyfill] Failed to parse mounts: ' + e);
    }

    // Remap guest path to host path (e.g., $HOME/.claude -> /tmp/edgebox-home/.claude)
    function _remapPath(path) {
        if (!path || typeof path !== 'string') return path;
        for (const mount of _edgeboxMounts) {
            // Exact match or path starts with mount.guest/
            if (path === mount.guest) {
                _log('[node_polyfill] Remapped ' + path + ' -> ' + mount.host);
                return mount.host;
            }
            if (path.startsWith(mount.guest + '/')) {
                const remapped = mount.host + path.slice(mount.guest.length);
                _log('[node_polyfill] Remapped ' + path + ' -> ' + remapped);
                return remapped;
            }
        }
        return path;
    }

    // ===== PATH MODULE =====
    _modules.path = {
        sep: '/',
        delimiter: ':',
        join: function(...parts) {
            return parts.filter(p => p).join('/').replace(/\/+/g, '/').replace(/\/$/, '') || '.';
        },
        resolve: function(...parts) {
            let resolved = '';
            for (let i = parts.length - 1; i >= 0; i--) {
                const part = parts[i];
                if (!part) continue;
                resolved = part + '/' + resolved;
                if (part.startsWith('/')) break;
            }
            return this.normalize(resolved || '/');
        },
        normalize: function(p) {
            const parts = p.split('/').filter(x => x && x !== '.');
            const result = [];
            for (const part of parts) {
                if (part === '..') {
                    if (result.length && result[result.length-1] !== '..') result.pop();
                    else if (!p.startsWith('/')) result.push('..');
                } else result.push(part);
            }
            return (p.startsWith('/') ? '/' : '') + result.join('/') || '.';
        },
        dirname: function(p) {
            const idx = p.lastIndexOf('/');
            if (idx === -1) return '.';
            if (idx === 0) return '/';
            return p.slice(0, idx);
        },
        basename: function(p, ext) {
            let base = p.slice(p.lastIndexOf('/') + 1);
            if (ext && base.endsWith(ext)) base = base.slice(0, -ext.length);
            return base;
        },
        extname: function(p) {
            const base = this.basename(p);
            const idx = base.lastIndexOf('.');
            return idx > 0 ? base.slice(idx) : '';
        },
        isAbsolute: function(p) { return p.startsWith('/'); },
        relative: function(from, to) {
            from = this.resolve(from).split('/');
            to = this.resolve(to).split('/');
            while (from.length && to.length && from[0] === to[0]) { from.shift(); to.shift(); }
            return [...from.map(() => '..'), ...to].join('/') || '.';
        },
        parse: function(p) {
            return { root: p.startsWith('/') ? '/' : '', dir: this.dirname(p), base: this.basename(p), ext: this.extname(p), name: this.basename(p, this.extname(p)) };
        },
        format: function(obj) {
            const dir = obj.dir || obj.root || '';
            const base = obj.base || (obj.name || '') + (obj.ext || '');
            return dir ? this.join(dir, base) : base;
        }
    };
    // Add posix and win32 aliases (WASM always uses posix-style paths)
    _modules.path.posix = _modules.path;
    _modules.path.win32 = Object.assign({}, _modules.path, { sep: '\\', delimiter: ';' });
    _modules['node:path'] = _modules.path;

    // ===== BUFFER CLASS =====
    class Buffer extends Uint8Array {
        static from(data, encoding) {
            if (typeof data === 'string') return new Buffer(new TextEncoder().encode(data));
            if (data instanceof ArrayBuffer) return new Buffer(new Uint8Array(data));
            if (Array.isArray(data) || data instanceof Uint8Array) return new Buffer(data);
            return new Buffer(0);
        }
        static alloc(size, fill) { const buf = new Buffer(size); if (fill !== undefined) buf.fill(fill); return buf; }
        static allocUnsafe(size) { return new Buffer(size); }
        static concat(list, totalLength) {
            if (totalLength === undefined) totalLength = list.reduce((sum, buf) => sum + buf.length, 0);
            const result = new Buffer(totalLength);
            let offset = 0;
            for (const buf of list) { result.set(buf, offset); offset += buf.length; }
            return result;
        }
        static isBuffer(obj) { return obj instanceof Buffer; }
        static byteLength(str) { return new TextEncoder().encode(str).length; }
        toString(encoding) { return new TextDecoder(encoding || 'utf-8').decode(this); }
        write(string, offset, length) {
            offset = offset || 0;
            const encoded = new TextEncoder().encode(string);
            const toWrite = length ? encoded.slice(0, length) : encoded;
            this.set(toWrite, offset);
            return toWrite.length;
        }
        slice(start, end) { return new Buffer(super.slice(start, end)); }
        copy(target, targetStart, sourceStart, sourceEnd) {
            const slice = this.slice(sourceStart || 0, sourceEnd || this.length);
            target.set(slice, targetStart || 0);
            return slice.length;
        }
        equals(other) {
            if (this.length !== other.length) return false;
            for (let i = 0; i < this.length; i++) if (this[i] !== other[i]) return false;
            return true;
        }
        compare(other) {
            const len = Math.min(this.length, other.length);
            for (let i = 0; i < len; i++) {
                if (this[i] < other[i]) return -1;
                if (this[i] > other[i]) return 1;
            }
            return this.length - other.length;
        }
    }
    _modules.buffer = { Buffer };
    globalThis.Buffer = Buffer;

    // ===== TextEncoder/TextDecoder POLYFILL (must be before util module) =====
    if (typeof globalThis.TextEncoder === 'undefined') {
        globalThis.TextEncoder = class TextEncoder {
            constructor(encoding = 'utf-8') { this.encoding = encoding; }
            encode(str) {
                const bytes = [];
                for (let i = 0; i < str.length; i++) {
                    let c = str.charCodeAt(i);
                    if (c < 0x80) bytes.push(c);
                    else if (c < 0x800) bytes.push(0xc0 | (c >> 6), 0x80 | (c & 0x3f));
                    else if (c < 0x10000) bytes.push(0xe0 | (c >> 12), 0x80 | ((c >> 6) & 0x3f), 0x80 | (c & 0x3f));
                    else bytes.push(0xf0 | (c >> 18), 0x80 | ((c >> 12) & 0x3f), 0x80 | ((c >> 6) & 0x3f), 0x80 | (c & 0x3f));
                }
                return new Uint8Array(bytes);
            }
            encodeInto(str, u8arr) {
                const encoded = this.encode(str);
                const len = Math.min(encoded.length, u8arr.length);
                u8arr.set(encoded.slice(0, len));
                return { read: str.length, written: len };
            }
        };
    }
    if (typeof globalThis.TextDecoder === 'undefined') {
        globalThis.TextDecoder = class TextDecoder {
            constructor(encoding = 'utf-8') { this.encoding = encoding; }
            decode(buffer) {
                const bytes = buffer instanceof Uint8Array ? buffer : new Uint8Array(buffer);
                let result = '', i = 0;
                while (i < bytes.length) {
                    const b = bytes[i++];
                    if (b < 0x80) result += String.fromCharCode(b);
                    else if ((b & 0xe0) === 0xc0) result += String.fromCharCode(((b & 0x1f) << 6) | (bytes[i++] & 0x3f));
                    else if ((b & 0xf0) === 0xe0) result += String.fromCharCode(((b & 0x0f) << 12) | ((bytes[i++] & 0x3f) << 6) | (bytes[i++] & 0x3f));
                    else if ((b & 0xf8) === 0xf0) { i += 3; result += '?'; }
                }
                return result;
            }
        };
    }

    // ===== UTIL MODULE =====
    _modules.util = {
        promisify: fn => (...args) => new Promise((resolve, reject) => fn(...args, (err, result) => err ? reject(err) : resolve(result))),
        callbackify: fn => (...args) => { const cb = args.pop(); fn(...args).then(r => cb(null, r)).catch(e => cb(e)); },
        format: (fmt, ...args) => {
            let i = 0;
            return fmt.replace(/%[sdjoO%]/g, m => {
                if (m === '%%') return '%';
                if (i >= args.length) return m;
                const arg = args[i++];
                switch (m) { case '%s': return String(arg); case '%d': return Number(arg); case '%j': case '%o': case '%O': return JSON.stringify(arg); default: return m; }
            });
        },
        inspect: obj => JSON.stringify(obj, null, 2),
        types: {
            isArray: Array.isArray,
            isBoolean: x => typeof x === 'boolean',
            isNull: x => x === null,
            isNumber: x => typeof x === 'number',
            isString: x => typeof x === 'string',
            isUndefined: x => x === undefined,
            isObject: x => typeof x === 'object' && x !== null,
            isFunction: x => typeof x === 'function',
            isPromise: x => x instanceof Promise
        },
        TextEncoder: globalThis.TextEncoder,
        TextDecoder: globalThis.TextDecoder
    };

    // ===== EVENTS MODULE =====
    class EventEmitter {
        constructor() { this._events = {}; this._maxListeners = 10; }
        on(event, listener) { if (!this._events[event]) this._events[event] = []; this._events[event].push(listener); return this; }
        addListener(event, listener) { return this.on(event, listener); }
        once(event, listener) {
            const wrapper = (...args) => { this.removeListener(event, wrapper); listener(...args); };
            wrapper._original = listener;
            return this.on(event, wrapper);
        }
        off(event, listener) { return this.removeListener(event, listener); }
        removeListener(event, listener) {
            if (!this._events[event]) return this;
            this._events[event] = this._events[event].filter(l => l !== listener && l._original !== listener);
            return this;
        }
        removeAllListeners(event) { if (event) delete this._events[event]; else this._events = {}; return this; }
        emit(event, ...args) {
            if (!this._events[event]) return false;
            const listeners = this._events[event].slice();
            for (let i = 0; i < listeners.length; i++) {
                const listener = listeners[i];
                if (typeof listener !== 'function') {
                    print('[emit] listener #' + i + ' for event "' + event + '" is not a function: ' + typeof listener);
                    continue;  // Skip invalid listeners
                }
                try {
                    listener(...args);
                } catch(e) {
                    print('[emit] listener #' + i + ' for event "' + event + '" threw: ' + e.message);
                    // Don't re-throw - continue to other listeners
                }
            }
            return true;
        }
        listenerCount(event) { return (this._events[event] && this._events[event].length) || 0; }
        listeners(event) { return (this._events[event] || []).map(l => l._original || l); }
        setMaxListeners(n) { this._maxListeners = n; return this; }
        getMaxListeners() { return this._maxListeners; }
    }
    _modules.events = EventEmitter;
    // Also expose as object with EventEmitter property for Node.js compat
    _modules.events.EventEmitter = EventEmitter;
    // Static setMaxListeners for AbortSignal (Node.js 15+)
    _modules.events.setMaxListeners = function(n, ...targets) {
        // For signals, this is a no-op in our polyfill since we don't track signal listeners
        // In real Node.js this sets a limit on listeners for the given targets
        for (const target of targets) {
            if (target && typeof target.setMaxListeners === 'function') {
                target.setMaxListeners(n);
            }
        }
    };
    _modules.events.getMaxListeners = function(target) {
        if (target && typeof target.getMaxListeners === 'function') {
            return target.getMaxListeners();
        }
        return 10;
    };
    _modules['node:events'] = _modules.events;

    // ===== STREAM MODULE =====
    class Stream extends EventEmitter {
        pipe(dest) { this.on('data', chunk => dest.write(chunk)); this.on('end', () => dest.end()); return dest; }
    }
    class Readable extends Stream {
        constructor() { super(); this._readableState = { ended: false, buffer: [] }; }
        read() { return null; }
        push(chunk) {
            if (chunk === null) { this._readableState.ended = true; this.emit('end'); }
            else this.emit('data', chunk);
            return true;
        }
        // Async iterator support - required by SDK for stream validation
        async *[Symbol.asyncIterator]() {
            const chunks = [];
            let ended = false;
            let resolveWait = null;

            this.on('data', (chunk) => {
                chunks.push(chunk);
                if (resolveWait) { resolveWait(); resolveWait = null; }
            });
            this.on('end', () => {
                ended = true;
                if (resolveWait) { resolveWait(); resolveWait = null; }
            });

            while (true) {
                if (chunks.length > 0) {
                    yield chunks.shift();
                } else if (ended) {
                    return;
                } else {
                    await new Promise(resolve => { resolveWait = resolve; });
                }
            }
        }
    }
    class Writable extends Stream {
        constructor() { super(); this._writableState = { ended: false }; }
        write(chunk, encoding, callback) {
            if (typeof encoding === 'function') callback = encoding;
            this._write(chunk, 'utf8', callback || (() => {}));
            return true;
        }
        _write(chunk, encoding, callback) { callback(); }
        end(chunk) { if (chunk) this.write(chunk); this._writableState.ended = true; this.emit('finish'); }
    }
    class Duplex extends Stream {
        constructor() { super(); this._readableState = { ended: false }; this._writableState = { ended: false }; }
        read() { return null; }
        write(chunk) { return true; }
        end() { this.emit('finish'); }
    }
    class Transform extends Duplex { _transform(chunk, encoding, callback) { callback(null, chunk); } }
    class PassThrough extends Transform {}
    // Stream module - export Stream class as default (like events exports EventEmitter)
    // Some code does `require("stream")` and uses it directly as base class
    _modules.stream = Stream;
    _modules.stream.Stream = Stream;
    _modules.stream.Readable = Readable;
    _modules.stream.Writable = Writable;
    _modules.stream.Duplex = Duplex;
    _modules.stream.Transform = Transform;
    _modules.stream.PassThrough = PassThrough;
    _modules['node:stream'] = _modules.stream;

    // ===== DOMAIN MODULE =====
    // Stub domain module (deprecated in Node.js, but some packages still use it)
    class Domain extends EventEmitter {
        constructor() {
            super();
            this.members = [];
        }
        add(emitter) { this.members.push(emitter); }
        remove(emitter) { this.members = this.members.filter(m => m !== emitter); }
        bind(callback) { return callback; }
        intercept(callback) { return callback; }
        run(fn) {
            try { fn(); }
            catch (e) { this.emit('error', e); }
        }
        dispose() { this.members = []; }
        enter() {}
        exit() {}
    }
    _modules.domain = {
        Domain,
        create: () => new Domain(),
        active: null,
    };
    _modules['node:domain'] = _modules.domain;

    // ===== FS MODULE =====
    // fs module uses either native __edgebox_fs_* functions or std/os fallback
    // Checks happen at call time, not at load time
    let _fileReadCount = 0;
    _modules.fs = {
        readFileSync: function(path, options) {
            path = _remapPath(path); // Mount remapping
            _fileReadCount++;
            if (_fileReadCount <= 30) _log('[FS] readFileSync #' + _fileReadCount + ': ' + path);
            const encoding = typeof options === 'string' ? options : (options && options.encoding);
            if (typeof globalThis.__edgebox_fs_read === 'function') {
                const data = globalThis.__edgebox_fs_read(path);
                return (encoding === 'utf8' || encoding === 'utf-8') ? data : Buffer.from(data);
            } else if (typeof std !== 'undefined' && std.loadFile) {
                const data = std.loadFile(path);
                if (data === null) { const err = new Error('ENOENT: ' + path); err.code = 'ENOENT'; throw err; }
                return (encoding === 'utf8' || encoding === 'utf-8') ? data : Buffer.from(data);
            }
            throw new Error('fs.readFileSync not implemented - __edgebox_fs_read=' + typeof globalThis.__edgebox_fs_read);
        },
        writeFileSync: function(pathOrFd, data, options) {
            if (typeof pathOrFd === 'string') pathOrFd = _remapPath(pathOrFd); // Mount remapping
            const strBuf = typeof data === 'string' ? data : String(data);
            // Handle file descriptor (number) vs path (string)
            if (typeof pathOrFd === 'number') {
                const fd = pathOrFd;
                const _osModule = globalThis._os || (typeof os !== 'undefined' ? os : null);
                // Real fd (< 100) - try os.write
                if (_osModule && typeof _osModule.write === 'function' && fd < 100) {
                    // os.write expects ArrayBuffer, convert string
                    const encoder = new TextEncoder();
                    const arrayBuf = encoder.encode(strBuf).buffer;
                    const written = _osModule.write(fd, arrayBuf, 0, strBuf.length);
                    return written;
                }
                // Pseudo-fd (>= 100) - buffer the data and write on close
                // Or if path is tracked, write directly to the path
                const trackedPath = globalThis._fdPaths ? globalThis._fdPaths[fd] : null;
                if (trackedPath && typeof globalThis.__edgebox_fs_write === 'function') {
                    return globalThis.__edgebox_fs_write(trackedPath, strBuf);
                }
                return;
            }
            if (typeof globalThis.__edgebox_fs_write === 'function') return globalThis.__edgebox_fs_write(pathOrFd, strBuf);
            if (typeof std !== 'undefined' && std.open) {
                const f = std.open(path, 'w'); if (!f) throw new Error('ENOENT: ' + path);
                f.puts(String(data)); f.close();
                return;
            }
            throw new Error('fs.writeFileSync not implemented');
        },
        appendFileSync: function(path, data, options) {
            if (typeof path === 'string') path = _remapPath(path); // Mount remapping
            const content = typeof data === 'string' ? data : String(data);

            // Handle file descriptor (number) - Node.js supports both path and fd
            if (typeof path === 'number') {
                return this.writeSync(path, content);
            }

            // Use native append if available (more efficient)
            if (typeof globalThis.__edgebox_fs_append === 'function') {
                return globalThis.__edgebox_fs_append(path, content);
            }
            // Fallback: Read existing content, append new data, write back
            let existing = '';
            try {
                existing = this.readFileSync(path, { encoding: 'utf8' });
            } catch(e) {
                // File doesn't exist, start fresh
            }
            const newContent = existing + content;
            return this.writeFileSync(path, newContent, options);
        },
        appendFile: function(path, data, options) {
            return Promise.resolve(this.appendFileSync(path, data, options));
        },
        existsSync: function(path) {
            path = _remapPath(path); // Mount remapping
            if (typeof globalThis.__edgebox_fs_exists === 'function') return globalThis.__edgebox_fs_exists(path);
            if (typeof _os !== 'undefined' && _os.stat) { try { return _os.stat(path)[1] === 0; } catch(e) { return false; } }
            return false;
        },
        mkdirSync: function(path, options) {
            path = _remapPath(path); // Mount remapping
            const recursive = (options && options.recursive) || false;
            if (typeof globalThis.__edgebox_fs_mkdir === 'function') return globalThis.__edgebox_fs_mkdir(path, recursive);
            if (typeof _os !== 'undefined' && _os.mkdir) { try { _os.mkdir(path); } catch(e) { if (!recursive) throw e; } return; }
            throw new Error('fs.mkdirSync not implemented');
        },
        readdirSync: function(path, options) {
            path = _remapPath(path); // Mount remapping
            let entries;
            if (typeof globalThis.__edgebox_fs_readdir === 'function') {
                entries = globalThis.__edgebox_fs_readdir(path);
            } else if (typeof _os !== 'undefined' && _os.readdir) {
                const r = _os.readdir(path);
                if (r[1] !== 0) { const err = new Error('ENOENT: ' + path); err.code = 'ENOENT'; throw err; }
                entries = r[0].filter(x => x !== '.' && x !== '..');
            } else {
                throw new Error('fs.readdirSync not implemented');
            }
            if (options && options.withFileTypes) {
                return entries.map(name => ({
                    name,
                    isFile: () => this.statSync(path + '/' + name).isFile(),
                    isDirectory: () => this.statSync(path + '/' + name).isDirectory(),
                    isSymbolicLink: () => false
                }));
            }
            return entries;
        },
        statSync: function(path) {
            path = _remapPath(path); // Mount remapping
            if (typeof globalThis.__edgebox_fs_stat === 'function') return globalThis.__edgebox_fs_stat(path);
            if (typeof _os !== 'undefined' && _os.stat) {
                const r = _os.stat(path);
                if (r[1] !== 0) { const err = new Error('ENOENT: ' + path); err.code = 'ENOENT'; throw err; }
                const s = r[0];
                return {
                    isFile: () => (s.mode & 0o170000) === 0o100000,
                    isDirectory: () => (s.mode & 0o170000) === 0o040000,
                    isSymbolicLink: () => (s.mode & 0o170000) === 0o120000,
                    size: s.size, mtime: new Date(s.mtime * 1000), mode: s.mode
                };
            }
            throw new Error('fs.statSync not implemented');
        },
        lstatSync: function(path) { return this.statSync(path); }, // statSync already does remap
        unlinkSync: function(path) {
            path = _remapPath(path); // Mount remapping
            if (typeof globalThis.__edgebox_fs_unlink === 'function') return globalThis.__edgebox_fs_unlink(path);
            if (typeof _os !== 'undefined' && _os.remove) { try { _os.remove(path); } catch(e) { const err = new Error('ENOENT: ' + path); err.code = 'ENOENT'; throw err; } return; }
            throw new Error('fs.unlinkSync not implemented');
        },
        rmdirSync: function(path, options) {
            path = _remapPath(path); // Mount remapping
            const recursive = (options && options.recursive) || false;
            if (typeof globalThis.__edgebox_fs_rmdir === 'function') return globalThis.__edgebox_fs_rmdir(path, recursive);
            if (typeof _os !== 'undefined' && _os.remove) { try { _os.remove(path); } catch(e) { throw new Error('ENOENT: ' + path); } return; }
            throw new Error('fs.rmdirSync not implemented');
        },
        rmSync: function(path, options) { return this.rmdirSync(path, options); }, // rmdirSync already does remap
        renameSync: function(oldPath, newPath) {
            oldPath = _remapPath(oldPath); // Mount remapping
            newPath = _remapPath(newPath);
            if (typeof globalThis.__edgebox_fs_rename === 'function') return globalThis.__edgebox_fs_rename(oldPath, newPath);
            if (typeof _os !== 'undefined' && _os.rename) { try { _os.rename(oldPath, newPath); } catch(e) { throw new Error('ENOENT: ' + oldPath); } return; }
            throw new Error('fs.renameSync not implemented');
        },
        copyFileSync: function(src, dest) {
            src = _remapPath(src); // Mount remapping
            dest = _remapPath(dest);
            if (typeof globalThis.__edgebox_fs_copy === 'function') return globalThis.__edgebox_fs_copy(src, dest);
            // Fallback: read and write (readFileSync/writeFileSync already remap, but we did it above too - harmless)
            this.writeFileSync(dest, this.readFileSync(src));
        },
        // realpathSync - returns the path as-is (no symlink resolution in WASI)
        realpathSync: Object.assign(function(path) { return path; }, { native: function(path) { return path; } }),
        realpath: Object.assign(function(path, opts, cb) {
            if (typeof opts === 'function') { cb = opts; opts = {}; }
            if (cb) cb(null, path);
            return Promise.resolve(path);
        }, { native: function(path, opts, cb) { if (typeof opts === 'function') { cb = opts; } if (cb) cb(null, path); } }),
        // accessSync - check if path exists, throw if not
        accessSync: function(path, mode) {
            path = _remapPath(path); // Mount remapping
            if (typeof globalThis.__edgebox_fs_exists === 'function') {
                if (!globalThis.__edgebox_fs_exists(path)) {
                    const err = new Error('ENOENT: no such file or directory, access \'' + path + '\'');
                    err.code = 'ENOENT';
                    throw err;
                }
            } else if (typeof _os !== 'undefined' && _os.stat) {
                try { if (_os.stat(path)[1] !== 0) throw new Error('ENOENT'); }
                catch(e) { const err = new Error('ENOENT: ' + path); err.code = 'ENOENT'; throw err; }
            }
        },
        // File descriptor operations using QuickJS _os module
        openSync: function(path, flags, mode) {
            path = _remapPath(path); // Mount remapping
            // Try to get QuickJS _os module
            const _osModule = globalThis._os || (typeof os !== 'undefined' ? os : null);
            // Map Node.js flags to POSIX flags
            let osFlags = 0;
            if (_osModule && typeof _osModule.open === 'function') {
                if (flags === 'r' || flags === 'rs' || !flags) osFlags = _osModule.O_RDONLY;
                else if (flags === 'r+') osFlags = _osModule.O_RDWR;
                else if (flags === 'w') osFlags = _osModule.O_WRONLY | _osModule.O_CREAT | _osModule.O_TRUNC;
                else if (flags === 'wx' || flags === 'xw') osFlags = _osModule.O_WRONLY | _osModule.O_CREAT | _osModule.O_EXCL;
                else if (flags === 'w+') osFlags = _osModule.O_RDWR | _osModule.O_CREAT | _osModule.O_TRUNC;
                else if (flags === 'a') osFlags = _osModule.O_WRONLY | _osModule.O_CREAT | _osModule.O_APPEND;
                else if (flags === 'a+') osFlags = _osModule.O_RDWR | _osModule.O_CREAT | _osModule.O_APPEND;
                else osFlags = _osModule.O_RDWR | _osModule.O_CREAT;

                const fd = _osModule.open(path, osFlags, mode || 0o666);
                if (fd < 0) {
                    const err = new Error('ENOENT: no such file or directory, open \'' + path + '\'');
                    err.code = 'ENOENT';
                    throw err;
                }
                // Store path for fstatSync lookup
                if (!globalThis._fdPaths) globalThis._fdPaths = {};
                globalThis._fdPaths[fd] = path;
                return fd;
            }
            // Fallback: use native fs functions (limited - no fd tracking)
            // Create a pseudo-fd that stores the path for later writeFileSync calls
            const pseudoFd = ++globalThis._nextPseudoFd || (globalThis._nextPseudoFd = 100);
            globalThis._nextPseudoFd = pseudoFd;
            if (!globalThis._fdPaths) globalThis._fdPaths = {};
            globalThis._fdPaths[pseudoFd] = path;
            globalThis._fdFlags = globalThis._fdFlags || {};
            globalThis._fdFlags[pseudoFd] = flags;
            return pseudoFd;
        },
        closeSync: function(fd) {
            const _osModule = globalThis._os || (typeof os !== 'undefined' ? os : null);
            if (_osModule && typeof _osModule.close === 'function' && fd < 100) {
                // Real fd - close via os module
                _osModule.close(fd);
            }
            // Clean up tracking
            if (globalThis._fdPaths) delete globalThis._fdPaths[fd];
            if (globalThis._fdFlags) delete globalThis._fdFlags[fd];
            if (globalThis._fdBuffers) delete globalThis._fdBuffers[fd];
        },
        readSync: function(fd, buffer, offset, length, position) {
            if (typeof _os !== 'undefined' && _os.read) {
                const result = _os.read(fd, buffer.buffer || buffer, offset, length);
                return result;
            }
            return 0;
        },
        writeSync: function(fd, buffer, offset, length, position) {
            if (typeof _os !== 'undefined' && _os.write) {
                const data = typeof buffer === 'string' ? buffer : buffer.toString();
                const result = _os.write(fd, data, offset || 0, length || data.length);
                return result;
            }
            return buffer ? buffer.length : 0;
        },
        fstatSync: function(fd) {
            const path = globalThis._fdPaths ? globalThis._fdPaths[fd] : null;
            if (path && typeof globalThis.__edgebox_fs_stat === 'function') {
                return globalThis.__edgebox_fs_stat(path);
            }
            return { isFile: () => true, isDirectory: () => false, size: 0 };
        },
        fsyncSync: function(fd) {
            // fsync is a no-op in WASI - data is flushed on close
        },
        // Stream factories
        createReadStream: function(path) {
            const content = globalThis.__edgebox_fs_read(path);
            const stream = new EventEmitter();
            stream.pipe = (dest) => { dest.write(content); dest.end(); return dest; };
            setTimeout(() => { stream.emit('data', content); stream.emit('end'); }, 0);
            return stream;
        },
        createWriteStream: function(path) {
            const chunks = [];
            return {
                write: (chunk) => { chunks.push(chunk); return true; },
                end: (chunk) => { if (chunk) chunks.push(chunk); globalThis.__edgebox_fs_write(path, chunks.join('')); },
                on: () => {}
            };
        },
        // Constants
        constants: { F_OK: 0, R_OK: 4, W_OK: 2, X_OK: 1, COPYFILE_EXCL: 1 },
        // Async versions - use true async API if available
        readFile: function(path, options) {
            path = _remapPath(path); // Mount remapping
            const self = this;
            const encoding = typeof options === 'string' ? options : (options && options.encoding);

            // Check for async file API
            if (typeof globalThis.__edgebox_file_read_start === 'function') {
                return new Promise((resolve, reject) => {
                    const requestId = globalThis.__edgebox_file_read_start(path);
                    if (requestId < 0) {
                        const err = new Error('ENOENT: no such file or directory, open \'' + path + '\'');
                        err.code = 'ENOENT';
                        reject(err);
                        return;
                    }

                    const pollForResult = () => {
                        print('[pollForResult] requestId=' + requestId + ' typeof poll=' + (typeof globalThis.__edgebox_file_poll));
                        const status = globalThis.__edgebox_file_poll(requestId);
                        print('[pollForResult] status=' + status);
                        if (status === 1) {
                            // Complete - get result
                            try {
                                const data = globalThis.__edgebox_file_result(requestId);
                                resolve((encoding === 'utf8' || encoding === 'utf-8') ? data : Buffer.from(data));
                            } catch (e) {
                                reject(e);
                            }
                        } else if (status < 0) {
                            const err = new Error('ENOENT: no such file or directory');
                            err.code = 'ENOENT';
                            reject(err);
                        } else {
                            // Still pending - poll again
                            setTimeout(pollForResult, 1);
                        }
                    };
                    print('[readFile] Starting poll for requestId=' + requestId);
                    setTimeout(pollForResult, 0);
                });
            }
            // Fallback to sync
            return Promise.resolve(self.readFileSync(path, options));
        },
        writeFile: function(path, data, options) {
            path = _remapPath(path); // Mount remapping
            const self = this;
            const buf = typeof data === 'string' ? data : String(data);

            // Check for async file API
            if (typeof globalThis.__edgebox_file_write_start === 'function') {
                return new Promise((resolve, reject) => {
                    const requestId = globalThis.__edgebox_file_write_start(path, buf);
                    if (requestId < 0) {
                        const err = new Error('EACCES: permission denied, open \'' + path + '\'');
                        err.code = 'EACCES';
                        reject(err);
                        return;
                    }

                    const pollForResult = () => {
                        const status = globalThis.__edgebox_file_poll(requestId);
                        if (status === 1) {
                            // Complete
                            try {
                                globalThis.__edgebox_file_result(requestId);
                                resolve();
                            } catch (e) {
                                reject(e);
                            }
                        } else if (status < 0) {
                            const err = new Error('EACCES: permission denied');
                            err.code = 'EACCES';
                            reject(err);
                        } else {
                            // Still pending - poll again
                            setTimeout(pollForResult, 1);
                        }
                    };
                    setTimeout(pollForResult, 0);
                });
            }
            // Fallback to sync
            return Promise.resolve(self.writeFileSync(path, data, options));
        },
        exists: function(path) { return Promise.resolve(this.existsSync(path)); },
        mkdir: function(path, options) { return Promise.resolve(this.mkdirSync(path, options)); },
        readdir: function(path, options) { return Promise.resolve(this.readdirSync(path, options)); },
        stat: function(path) { return Promise.resolve(this.statSync(path)); },
        lstat: function(path) { return Promise.resolve(this.lstatSync(path)); },
        unlink: function(path) { return Promise.resolve(this.unlinkSync(path)); },
        rmdir: function(path, options) { return Promise.resolve(this.rmdirSync(path, options)); },
        rm: function(path, options) { return Promise.resolve(this.rmSync(path, options)); },
        rename: function(oldPath, newPath) { return Promise.resolve(this.renameSync(oldPath, newPath)); },
        copyFile: function(src, dest) { return Promise.resolve(this.copyFileSync(src, dest)); },
        access: function(path, mode) { return Promise.resolve(this.accessSync(path, mode)); },
        // File watching stubs - not supported in WASI but needs to return valid watcher objects
        watch: function(path, options, listener) {
            if (typeof options === 'function') { listener = options; options = {}; }
            // Return a fake FSWatcher that does nothing
            const watcher = new EventEmitter();
            watcher.close = function() {};
            watcher.ref = function() { return this; };
            watcher.unref = function() { return this; };
            return watcher;
        },
        watchFile: function(path, options, listener) {
            if (typeof options === 'function') { listener = options; options = {}; }
            // No-op - file watching not supported
        },
        unwatchFile: function(path, listener) {
            // No-op
        },
        promises: null
    };
    _modules.fs.promises = {
        readFile: _modules.fs.readFile.bind(_modules.fs),
        writeFile: _modules.fs.writeFile.bind(_modules.fs),
        mkdir: _modules.fs.mkdir.bind(_modules.fs),
        readdir: _modules.fs.readdir.bind(_modules.fs),
        stat: _modules.fs.stat.bind(_modules.fs),
        lstat: _modules.fs.lstat.bind(_modules.fs),
        unlink: _modules.fs.unlink.bind(_modules.fs),
        rmdir: _modules.fs.rmdir.bind(_modules.fs),
        rename: _modules.fs.rename.bind(_modules.fs),
        copyFile: _modules.fs.copyFile.bind(_modules.fs),
        access: path => Promise.resolve(_modules.fs.existsSync(path)),
        realpath: path => Promise.resolve(_modules.fs.realpathSync(path))
    };
    _modules['fs/promises'] = _modules.fs.promises;

    // ===== CRYPTO MODULE =====
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

    // ===== HTTP MODULE =====
    class IncomingMessage extends EventEmitter {
        constructor() { super(); this.headers = {}; this.statusCode = 200; this.statusMessage = 'OK'; }
    }
    class ServerResponse extends EventEmitter {
        constructor() { super(); this.statusCode = 200; this._headers = {}; this._body = []; }
        setHeader(name, value) { this._headers[name.toLowerCase()] = value; }
        getHeader(name) { return this._headers[name.toLowerCase()]; }
        writeHead(status, headers) { this.statusCode = status; Object.assign(this._headers, headers); }
        write(chunk) { this._body.push(chunk); return true; }
        end(data) { if (data) this._body.push(data); this.emit('finish'); }
    }
    // HTTP Agent class for connection pooling
    class Agent extends EventEmitter {
        constructor(options = {}) {
            super();
            this.options = options;
            this.keepAlive = options.keepAlive || false;
            this.keepAliveMsecs = options.keepAliveMsecs || 1000;
            this.maxSockets = options.maxSockets || Infinity;
            this.maxFreeSockets = options.maxFreeSockets || 256;
            this.maxTotalSockets = options.maxTotalSockets || Infinity;
            this.scheduling = options.scheduling || 'lifo';
            this.timeout = options.timeout;
            this.sockets = {};
            this.freeSockets = {};
            this.requests = {};
        }
        createConnection(options, callback) {
            // Stub - in WASM we use fetch instead of sockets
            if (callback) setTimeout(callback, 0);
            return new EventEmitter();
        }
        getName(options) {
            return `${options.host || options.hostname || 'localhost'}:${options.port || 80}:${options.localAddress || ''}`;
        }
        destroy() {
            this.sockets = {};
            this.freeSockets = {};
            this.requests = {};
        }
    }

    _modules.http = {
        IncomingMessage, ServerResponse, Agent,
        globalAgent: new Agent(),
        request: function(options, callback) {
            const url = typeof options === 'string' ? options : (options.protocol || 'http:') + '//' + (options.hostname || options.host) + (options.path || '/');
            const req = new EventEmitter();
            req._body = [];
            req.write = chunk => { req._body.push(chunk); return true; };
            req.end = data => {
                if (data) req._body.push(data);
                fetch(url, { method: options.method || 'GET', headers: options.headers, body: req._body.length ? req._body.join('') : undefined })
                    .then(async response => {
                        const res = new IncomingMessage();
                        res.statusCode = response.status;
                        res.headers = Object.fromEntries(response.headers);
                        if (callback) callback(res);
                        req.emit('response', res);
                        const text = await response.text();
                        res.emit('data', text);
                        res.emit('end');
                    }).catch(err => req.emit('error', err));
            };
            return req;
        },
        get: function(options, callback) {
            if (typeof options === 'string') options = { url: options };
            options.method = 'GET';
            const req = this.request(options, callback);
            req.end();
            return req;
        },
        createServer: function(options, requestListener) {
            if (typeof options === 'function') {
                requestListener = options;
                options = {};
            }
            const net = _modules.net;
            const server = net.createServer(function(socket) {
                var buffer = '';
                socket.on('data', function(chunk) {
                    buffer += chunk.toString();
                    // Check if we have a complete HTTP request (headers end with \r\n\r\n)
                    var headerEnd = buffer.indexOf('\r\n\r\n');
                    if (headerEnd === -1) return;

                    var headerPart = buffer.substring(0, headerEnd);
                    var bodyPart = buffer.substring(headerEnd + 4);
                    var lines = headerPart.split('\r\n');
                    var requestLine = lines[0].split(' ');

                    // Parse request
                    var req = new IncomingMessage();
                    req.method = requestLine[0];
                    req.url = requestLine[1];
                    req.httpVersion = (requestLine[2] || 'HTTP/1.1').replace('HTTP/', '');
                    req.headers = {};
                    req.socket = socket;
                    req.connection = socket;

                    // Parse headers
                    for (var i = 1; i < lines.length; i++) {
                        var colonIdx = lines[i].indexOf(':');
                        if (colonIdx > 0) {
                            var key = lines[i].substring(0, colonIdx).toLowerCase().trim();
                            var val = lines[i].substring(colonIdx + 1).trim();
                            req.headers[key] = val;
                        }
                    }

                    // Create response
                    var res = new ServerResponse();
                    res.socket = socket;
                    res.connection = socket;
                    res._headerSent = false;
                    res._headers = { 'content-type': 'text/html' };
                    res._statusCode = 200;

                    res.writeHead = function(statusCode, statusMessage, headers) {
                        if (typeof statusMessage === 'object') {
                            headers = statusMessage;
                            statusMessage = null;
                        }
                        res._statusCode = statusCode;
                        if (headers) {
                            for (var k in headers) {
                                // Security: Protect against prototype pollution
                                if (Object.prototype.hasOwnProperty.call(headers, k)) {
                                    res._headers[k.toLowerCase()] = headers[k];
                                }
                            }
                        }
                    };

                    res.setHeader = function(name, value) {
                        res._headers[name.toLowerCase()] = value;
                    };

                    res.getHeader = function(name) {
                        return res._headers[name.toLowerCase()];
                    };

                    res.removeHeader = function(name) {
                        delete res._headers[name.toLowerCase()];
                    };

                    res.write = function(chunk) {
                        if (!res._headerSent) {
                            var statusText = _modules.http.STATUS_CODES[res._statusCode] || 'Unknown';
                            var headerLines = ['HTTP/1.1 ' + res._statusCode + ' ' + statusText];
                            for (var k in res._headers) {
                                headerLines.push(k + ': ' + res._headers[k]);
                            }
                            headerLines.push('', '');
                            socket.write(headerLines.join('\r\n'));
                            res._headerSent = true;
                        }
                        if (chunk) socket.write(typeof chunk === 'string' ? chunk : chunk.toString());
                        return true;
                    };

                    res.end = function(chunk) {
                        if (chunk) res.write(chunk);
                        else if (!res._headerSent) res.write('');
                        socket.end();
                        res.emit('finish');
                    };

                    // Handle body for POST/PUT
                    var contentLength = parseInt(req.headers['content-length'] || '0', 10);
                    if (contentLength > 0 && bodyPart.length >= contentLength) {
                        // Body complete
                        setTimeout(function() {
                            req.emit('data', bodyPart.substring(0, contentLength));
                            req.emit('end');
                        }, 0);
                    } else if (contentLength === 0) {
                        setTimeout(function() { req.emit('end'); }, 0);
                    }

                    buffer = '';
                    if (requestListener) requestListener(req, res);
                    server.emit('request', req, res);
                });

                socket.on('error', function(err) {
                    server.emit('clientError', err, socket);
                });
            });

            // Forward server events
            server.setTimeout = function(ms, callback) {
                if (callback) server.on('timeout', callback);
                return server;
            };

            return server;
        },
        METHODS: ['GET', 'HEAD', 'POST', 'PUT', 'DELETE', 'CONNECT', 'OPTIONS', 'TRACE', 'PATCH'],
        STATUS_CODES: {
            100: 'Continue', 101: 'Switching Protocols', 200: 'OK', 201: 'Created',
            204: 'No Content', 301: 'Moved Permanently', 302: 'Found', 304: 'Not Modified',
            400: 'Bad Request', 401: 'Unauthorized', 403: 'Forbidden', 404: 'Not Found',
            500: 'Internal Server Error', 502: 'Bad Gateway', 503: 'Service Unavailable'
        }
    };
    _modules['node:http'] = _modules.http;
    _modules.https = Object.assign({}, _modules.http, {
        globalAgent: new Agent({ keepAlive: true }),
    });
    _modules['node:https'] = _modules.https;

    // ===== HTTP2 MODULE =====
    // HTTP/2 client implementation using native bindings
    // Server-side HTTP/2 uses net sockets (no ALPN negotiation)

    var _h2ConnectionId = 0;
    var _h2StreamId = 0;

    // HTTP/2 Session (Client)
    class Http2Session extends EventEmitter {
        constructor(authority, options) {
            super();
            var self = this;
            this._id = ++_h2ConnectionId;
            this._authority = authority;
            this._options = options || {};
            this._socket = null;
            this._streams = new Map();
            this._closed = false;
            this._nextStreamId = 1;
            this._settings = {
                headerTableSize: 4096,
                enablePush: true,
                maxConcurrentStreams: 100,
                initialWindowSize: 65535,
                maxFrameSize: 16384,
                maxHeaderListSize: 8192
            };

            // Parse authority URL
            var url = new URL(authority.startsWith('http') ? authority : 'https://' + authority);
            this._host = url.hostname;
            this._port = parseInt(url.port) || (url.protocol === 'https:' ? 443 : 80);
            this._secure = url.protocol === 'https:';

            // Connect via TCP socket
            var net = _modules.net;
            this._socket = net.connect({ host: this._host, port: this._port }, function() {
                // Send HTTP/2 connection preface
                self._sendPreface();
                self.emit('connect', self);
            });

            this._socket.on('data', function(data) {
                self._handleData(data);
            });

            this._socket.on('error', function(err) {
                self.emit('error', err);
            });

            this._socket.on('close', function() {
                self._closed = true;
                self.emit('close');
            });
        }

        _sendPreface() {
            // Connection preface
            this._socket.write('PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n');
            // Send SETTINGS frame
            this._sendSettings();
        }

        _sendSettings() {
            // Build SETTINGS frame
            var payload = Buffer.alloc(18); // 3 settings * 6 bytes each
            var pos = 0;
            // SETTINGS_MAX_CONCURRENT_STREAMS
            payload.writeUInt16BE(0x3, pos); pos += 2;
            payload.writeUInt32BE(100, pos); pos += 4;
            // SETTINGS_INITIAL_WINDOW_SIZE
            payload.writeUInt16BE(0x4, pos); pos += 2;
            payload.writeUInt32BE(65535, pos); pos += 4;
            // SETTINGS_MAX_FRAME_SIZE
            payload.writeUInt16BE(0x5, pos); pos += 2;
            payload.writeUInt32BE(16384, pos); pos += 4;

            this._sendFrame(0x4, 0, 0, payload.slice(0, pos)); // SETTINGS frame
        }

        _sendFrame(type, flags, streamId, payload) {
            var header = Buffer.alloc(9);
            var len = payload ? payload.length : 0;
            header.writeUIntBE(len, 0, 3);
            header.writeUInt8(type, 3);
            header.writeUInt8(flags, 4);
            header.writeUInt32BE(streamId & 0x7FFFFFFF, 5);
            this._socket.write(header);
            if (payload && payload.length > 0) {
                this._socket.write(payload);
            }
        }

        _handleData(data) {
            // Parse HTTP/2 frames
            var pos = 0;
            while (pos + 9 <= data.length) {
                var len = (data[pos] << 16) | (data[pos+1] << 8) | data[pos+2];
                var type = data[pos+3];
                var flags = data[pos+4];
                var streamId = ((data[pos+5] & 0x7F) << 24) | (data[pos+6] << 16) | (data[pos+7] << 8) | data[pos+8];
                pos += 9;

                if (pos + len > data.length) break;
                var payload = data.slice(pos, pos + len);
                pos += len;

                this._handleFrame(type, flags, streamId, payload);
            }
        }

        _handleFrame(type, flags, streamId, payload) {
            switch (type) {
                case 0x0: // DATA
                    var stream = this._streams.get(streamId);
                    if (stream) {
                        stream.emit('data', payload);
                        if (flags & 0x1) { // END_STREAM
                            stream.emit('end');
                        }
                    }
                    break;
                case 0x1: // HEADERS
                    var stream = this._streams.get(streamId);
                    if (stream) {
                        // Parse HPACK headers (simplified - just emit raw for now)
                        stream.emit('response', { ':status': '200' }, flags);
                        if (flags & 0x1) { // END_STREAM
                            stream.emit('end');
                        }
                    }
                    break;
                case 0x4: // SETTINGS
                    if (!(flags & 0x1)) { // Not ACK
                        // Send SETTINGS ACK
                        this._sendFrame(0x4, 0x1, 0, Buffer.alloc(0));
                    }
                    this.emit('remoteSettings', this._settings);
                    break;
                case 0x6: // PING
                    if (!(flags & 0x1)) {
                        // Send PING ACK
                        this._sendFrame(0x6, 0x1, 0, payload);
                    }
                    break;
                case 0x7: // GOAWAY
                    this.emit('goaway', payload);
                    break;
            }
        }

        request(headers, options) {
            var self = this;
            var streamId = this._nextStreamId;
            this._nextStreamId += 2; // Client uses odd stream IDs

            var stream = new Http2Stream(this, streamId);
            this._streams.set(streamId, stream);

            // Encode headers (simplified HPACK - literal without indexing)
            var headerBuf = [];
            for (var name in headers) {
                var value = headers[name];
                // Literal header field without indexing
                headerBuf.push(0x00);
                headerBuf.push(name.length);
                for (var i = 0; i < name.length; i++) headerBuf.push(name.charCodeAt(i));
                headerBuf.push(value.length);
                for (var i = 0; i < value.length; i++) headerBuf.push(value.charCodeAt(i));
            }

            var payload = Buffer.from(headerBuf);
            var flags = 0x4; // END_HEADERS
            if (!options || !options.endStream === false) {
                // Will send data separately
            }

            this._sendFrame(0x1, flags, streamId, payload); // HEADERS frame

            return stream;
        }

        close(callback) {
            var self = this;
            // Send GOAWAY
            var payload = Buffer.alloc(8);
            payload.writeUInt32BE(this._nextStreamId - 2, 0); // Last stream ID
            payload.writeUInt32BE(0, 4); // NO_ERROR
            this._sendFrame(0x7, 0, 0, payload);

            this._closed = true;
            if (this._socket) {
                this._socket.end();
            }
            if (callback) setTimeout(callback, 0);
        }

        get closed() { return this._closed; }
        get destroyed() { return this._closed; }
        get encrypted() { return this._secure; }
        get alpnProtocol() { return 'h2'; }
        get originSet() { return [this._authority]; }
        get pendingSettingsAck() { return false; }
        get remoteSettings() { return this._settings; }
        get localSettings() { return this._settings; }
        get socket() { return this._socket; }
        get state() { return { effectiveLocalWindowSize: 65535, effectiveRecvDataLength: 0, nextStreamID: this._nextStreamId, localWindowSize: 65535, lastProcStreamID: 0, remoteWindowSize: 65535, deflateDynamicTableSize: 4096, inflateDynamicTableSize: 4096 }; }
        get type() { return 1; } // NGHTTP2_SESSION_CLIENT

        ping(payload, callback) {
            this._sendFrame(0x6, 0, 0, payload || Buffer.alloc(8));
            if (callback) setTimeout(function() { callback(null, 0, payload); }, 10);
            return true;
        }

        settings(settings, callback) {
            if (settings) Object.assign(this._settings, settings);
            this._sendSettings();
            if (callback) setTimeout(callback, 0);
        }

        goaway(code, lastStreamId, data) {
            var payload = Buffer.alloc(8 + (data ? data.length : 0));
            payload.writeUInt32BE(lastStreamId || 0, 0);
            payload.writeUInt32BE(code || 0, 4);
            if (data) data.copy(payload, 8);
            this._sendFrame(0x7, 0, 0, payload);
        }

        destroy(err) {
            this._closed = true;
            if (this._socket) this._socket.destroy();
            if (err) this.emit('error', err);
            this.emit('close');
        }

        ref() { return this; }
        unref() { return this; }
        setTimeout(ms, callback) { if (callback) this.on('timeout', callback); return this; }
        setLocalWindowSize(size) {}
    }

    // HTTP/2 Stream
    class Http2Stream extends EventEmitter {
        constructor(session, id) {
            super();
            this._session = session;
            this._id = id;
            this._closed = false;
            this._sentHeaders = false;
            this._sentTrailers = false;
            this._state = 'open';
            this.rstCode = 0;
        }

        write(data, encoding, callback) {
            if (typeof encoding === 'function') { callback = encoding; encoding = 'utf8'; }
            var buf = Buffer.isBuffer(data) ? data : Buffer.from(data, encoding);
            this._session._sendFrame(0x0, 0, this._id, buf); // DATA frame
            if (callback) setTimeout(callback, 0);
            return true;
        }

        end(data, encoding, callback) {
            if (typeof data === 'function') { callback = data; data = null; }
            if (typeof encoding === 'function') { callback = encoding; encoding = 'utf8'; }

            var flags = 0x1; // END_STREAM
            var buf = data ? (Buffer.isBuffer(data) ? data : Buffer.from(data, encoding)) : Buffer.alloc(0);
            this._session._sendFrame(0x0, flags, this._id, buf);

            this._closed = true;
            this._state = 'closed';
            if (callback) setTimeout(callback, 0);
        }

        close(code, callback) {
            if (typeof code === 'function') { callback = code; code = 0; }
            // Send RST_STREAM
            var payload = Buffer.alloc(4);
            payload.writeUInt32BE(code || 0, 0);
            this._session._sendFrame(0x3, 0, this._id, payload);
            this._closed = true;
            this._state = 'closed';
            this.rstCode = code || 0;
            if (callback) setTimeout(callback, 0);
        }

        get id() { return this._id; }
        get pending() { return !this._sentHeaders; }
        get destroyed() { return this._closed; }
        get closed() { return this._closed; }
        get aborted() { return false; }
        get session() { return this._session; }
        get sentHeaders() { return this._sentHeaders; }
        get sentTrailers() { return this._sentTrailers; }
        get state() { return this._state; }

        priority(options) {}
        setTimeout(ms, callback) { if (callback) this.on('timeout', callback); return this; }
        sendTrailers(headers) { this._sentTrailers = true; }
    }

    // HTTP/2 Server Session (incoming)
    class Http2ServerSession extends Http2Session {
        constructor(socket, options) {
            super('', options);
            this._socket = socket;
            this._nextStreamId = 2; // Server uses even stream IDs
        }
    }

    _modules.http2 = {
        constants: {
            HTTP2_HEADER_METHOD: ':method',
            HTTP2_HEADER_PATH: ':path',
            HTTP2_HEADER_STATUS: ':status',
            HTTP2_HEADER_AUTHORITY: ':authority',
            HTTP2_HEADER_SCHEME: ':scheme',
            HTTP2_HEADER_CONTENT_TYPE: 'content-type',
            HTTP2_HEADER_CONTENT_LENGTH: 'content-length',
            HTTP2_HEADER_ACCEPT: 'accept',
            HTTP2_HEADER_ACCEPT_ENCODING: 'accept-encoding',
            HTTP2_HEADER_USER_AGENT: 'user-agent',
            // Error codes
            NGHTTP2_NO_ERROR: 0,
            NGHTTP2_PROTOCOL_ERROR: 1,
            NGHTTP2_INTERNAL_ERROR: 2,
            NGHTTP2_FLOW_CONTROL_ERROR: 3,
            NGHTTP2_SETTINGS_TIMEOUT: 4,
            NGHTTP2_STREAM_CLOSED: 5,
            NGHTTP2_FRAME_SIZE_ERROR: 6,
            NGHTTP2_REFUSED_STREAM: 7,
            NGHTTP2_CANCEL: 8,
            NGHTTP2_COMPRESSION_ERROR: 9,
            NGHTTP2_CONNECT_ERROR: 10,
            NGHTTP2_ENHANCE_YOUR_CALM: 11,
            NGHTTP2_INADEQUATE_SECURITY: 12,
            // Settings
            NGHTTP2_DEFAULT_WEIGHT: 16,
            HTTP2_HEADER_COOKIE: 'cookie',
            HTTP2_HEADER_SET_COOKIE: 'set-cookie',
        },
        connect: function(authority, options, listener) {
            if (typeof options === 'function') {
                listener = options;
                options = {};
            }
            var session = new Http2Session(authority, options);
            if (listener) session.on('connect', listener);
            return session;
        },
        createServer: function(options, onRequestHandler) {
            if (typeof options === 'function') {
                onRequestHandler = options;
                options = {};
            }
            var net = _modules.net;
            var server = net.createServer(function(socket) {
                var session = new Http2ServerSession(socket, options);
                if (onRequestHandler) {
                    session.on('stream', function(stream, headers) {
                        onRequestHandler(stream, headers);
                    });
                }
            });
            server.setTimeout = function(ms, callback) { if (callback) server.on('timeout', callback); return server; };
            return server;
        },
        createSecureServer: function(options, onRequestHandler) {
            // For now, same as createServer (TLS would wrap the socket)
            return _modules.http2.createServer(options, onRequestHandler);
        },
        getDefaultSettings: function() {
            return {
                headerTableSize: 4096,
                enablePush: true,
                maxConcurrentStreams: 100,
                initialWindowSize: 65535,
                maxFrameSize: 16384,
                maxHeaderListSize: 8192
            };
        },
        getPackedSettings: function(settings) {
            var buf = Buffer.alloc(36);
            var pos = 0;
            if (settings.headerTableSize !== undefined) {
                buf.writeUInt16BE(0x1, pos); buf.writeUInt32BE(settings.headerTableSize, pos + 2); pos += 6;
            }
            if (settings.maxConcurrentStreams !== undefined) {
                buf.writeUInt16BE(0x3, pos); buf.writeUInt32BE(settings.maxConcurrentStreams, pos + 2); pos += 6;
            }
            if (settings.initialWindowSize !== undefined) {
                buf.writeUInt16BE(0x4, pos); buf.writeUInt32BE(settings.initialWindowSize, pos + 2); pos += 6;
            }
            if (settings.maxFrameSize !== undefined) {
                buf.writeUInt16BE(0x5, pos); buf.writeUInt32BE(settings.maxFrameSize, pos + 2); pos += 6;
            }
            return buf.slice(0, pos);
        },
        getUnpackedSettings: function(buffer) {
            var settings = {};
            for (var i = 0; i + 6 <= buffer.length; i += 6) {
                var id = buffer.readUInt16BE(i);
                var value = buffer.readUInt32BE(i + 2);
                switch (id) {
                    case 0x1: settings.headerTableSize = value; break;
                    case 0x2: settings.enablePush = value === 1; break;
                    case 0x3: settings.maxConcurrentStreams = value; break;
                    case 0x4: settings.initialWindowSize = value; break;
                    case 0x5: settings.maxFrameSize = value; break;
                    case 0x6: settings.maxHeaderListSize = value; break;
                }
            }
            return settings;
        },
        sensitiveHeaders: Symbol('nodejs.http2.sensitiveHeaders'),
        Http2Session: Http2Session,
        Http2Stream: Http2Stream,
        Http2ServerSession: Http2ServerSession,
    };
    _modules['node:http2'] = _modules.http2;

    // ===== NET MODULE =====
    // Real socket implementation using Unix domain sockets via host bridge
    // Socket states: 0=created, 1=bound, 2=listening, 3=connected, 4=closed
    const SOCKET_STATE = { CREATED: 0, BOUND: 1, LISTENING: 2, CONNECTED: 3, CLOSED: 4 };

    class Socket extends EventEmitter {
        constructor(options = {}) {
            super();
            this._socketId = null;
            this._encoding = null;
            this._readPollInterval = null;
            this.connecting = false;
            this.destroyed = false;
            this.readable = true;
            this.writable = true;
            this.remoteAddress = '127.0.0.1';
            this.remotePort = null;
            this.localAddress = '127.0.0.1';
            this.localPort = null;
            this.bytesRead = 0;
            this.bytesWritten = 0;
            this.pending = true;
            this.readyState = 'opening';

            // If fd provided, wrap existing socket
            if (options.fd !== undefined) {
                this._socketId = options.fd;
                this.pending = false;
                this.readyState = 'open';
                this._startReadPolling();
            }
        }

        _createSocket() {
            if (this._socketId === null) {
                this._socketId = __edgebox_socket_create();
                if (this._socketId < 0) {
                    throw new Error('Failed to create socket');
                }
            }
        }

        _startReadPolling() {
            if (this._readPollInterval) return;
            this._readPollInterval = setInterval(() => {
                if (this.destroyed || !this._socketId) {
                    this._stopReadPolling();
                    return;
                }
                const state = __edgebox_socket_state(this._socketId);
                if (state === SOCKET_STATE.CLOSED) {
                    this._stopReadPolling();
                    this.emit('end');
                    this.emit('close', false);
                    return;
                }
                if (state !== SOCKET_STATE.CONNECTED) return;

                const data = __edgebox_socket_read(this._socketId, 65536);
                if (data === null) {
                    // EOF - peer closed connection
                    this._stopReadPolling();
                    this.readable = false;
                    this.emit('end');
                    this.emit('close', false);
                    return;
                }
                if (data && data.length > 0) {
                    this.bytesRead += data.length;
                    const chunk = this._encoding ? data : Buffer.from(data);
                    this.emit('data', chunk);
                }
            }, 10);
        }

        _stopReadPolling() {
            if (this._readPollInterval) {
                clearInterval(this._readPollInterval);
                this._readPollInterval = null;
            }
        }

        connect(optionsOrPort, hostOrCallback, maybeCallback) {
            let port, host, callback;
            if (typeof optionsOrPort === 'object') {
                port = optionsOrPort.port;
                host = optionsOrPort.host || '127.0.0.1';
                callback = hostOrCallback;
            } else {
                port = optionsOrPort;
                host = typeof hostOrCallback === 'string' ? hostOrCallback : '127.0.0.1';
                callback = typeof hostOrCallback === 'function' ? hostOrCallback : maybeCallback;
            }

            this.connecting = true;
            this.remotePort = port;
            this.remoteAddress = host;
            if (callback) this.once('connect', callback);

            setTimeout(() => {
                try {
                    this._createSocket();
                    const result = __edgebox_socket_connect(this._socketId, port);
                    if (result < 0) {
                        this.connecting = false;
                        this.emit('error', new Error(`Connection failed: ${result}`));
                        return;
                    }
                    this.connecting = false;
                    this.pending = false;
                    this.readyState = 'open';
                    this._startReadPolling();
                    this.emit('connect');
                    this.emit('ready');
                } catch (err) {
                    this.connecting = false;
                    this.emit('error', err);
                }
            }, 0);

            return this;
        }

        write(data, encoding, callback) {
            if (typeof encoding === 'function') {
                callback = encoding;
                encoding = null;
            }
            if (this.destroyed || !this.writable) {
                const err = new Error('Socket is not writable');
                if (callback) setTimeout(() => callback(err), 0);
                return false;
            }
            if (!this._socketId) {
                const err = new Error('Socket not connected');
                if (callback) setTimeout(() => callback(err), 0);
                return false;
            }

            const str = typeof data === 'string' ? data : data.toString(encoding || 'utf8');
            const result = __edgebox_socket_write(this._socketId, str);
            if (result < 0) {
                const err = new Error(`Write failed: ${result}`);
                if (callback) setTimeout(() => callback(err), 0);
                this.emit('error', err);
                return false;
            }
            this.bytesWritten += result;
            if (callback) setTimeout(callback, 0);
            return true;
        }

        end(data, encoding, callback) {
            if (typeof data === 'function') {
                callback = data;
                data = null;
            } else if (typeof encoding === 'function') {
                callback = encoding;
                encoding = null;
            }

            if (data) this.write(data, encoding);
            this.writable = false;
            this.readyState = 'writeOnly';

            if (callback) this.once('finish', callback);
            setTimeout(() => this.emit('finish'), 0);
            return this;
        }

        destroy(error) {
            if (this.destroyed) return this;
            this.destroyed = true;
            this.readable = false;
            this.writable = false;
            this.readyState = 'closed';
            this._stopReadPolling();

            if (this._socketId !== null) {
                __edgebox_socket_close(this._socketId);
                this._socketId = null;
            }

            if (error) this.emit('error', error);
            this.emit('close', !!error);
            return this;
        }

        setEncoding(encoding) {
            this._encoding = encoding;
            return this;
        }
        setKeepAlive(enable, delay) { return this; }
        setNoDelay(noDelay) { return this; }
        setTimeout(timeout, callback) {
            if (callback) this.once('timeout', callback);
            return this;
        }
        ref() { return this; }
        unref() { return this; }
        address() {
            return { port: this.localPort, address: this.localAddress, family: 'IPv4' };
        }
        pause() { this._stopReadPolling(); return this; }
        resume() { this._startReadPolling(); return this; }
    }

    class Server extends EventEmitter {
        constructor(options, connectionListener) {
            super();
            if (typeof options === 'function') {
                connectionListener = options;
                options = {};
            }
            this._options = options || {};
            this._socketId = null;
            this._acceptPollInterval = null;
            this._connections = new Set();
            this.listening = false;
            this.maxConnections = 0;

            if (connectionListener) {
                this.on('connection', connectionListener);
            }
        }

        listen(optionsOrPort, hostOrBacklogOrCallback, backlogOrCallback, maybeCallback) {
            let port, host, backlog, callback;

            if (typeof optionsOrPort === 'object') {
                port = optionsOrPort.port;
                host = optionsOrPort.host || '0.0.0.0';
                backlog = optionsOrPort.backlog || 511;
                callback = hostOrBacklogOrCallback;
            } else {
                port = optionsOrPort;
                if (typeof hostOrBacklogOrCallback === 'string') {
                    host = hostOrBacklogOrCallback;
                    backlog = typeof backlogOrCallback === 'number' ? backlogOrCallback : 511;
                    callback = typeof backlogOrCallback === 'function' ? backlogOrCallback : maybeCallback;
                } else if (typeof hostOrBacklogOrCallback === 'number') {
                    host = '0.0.0.0';
                    backlog = hostOrBacklogOrCallback;
                    callback = backlogOrCallback;
                } else {
                    host = '0.0.0.0';
                    backlog = 511;
                    callback = hostOrBacklogOrCallback;
                }
            }

            if (callback) this.once('listening', callback);

            setTimeout(() => {
                try {
                    this._socketId = __edgebox_socket_create();
                    if (this._socketId < 0) {
                        throw new Error('Failed to create server socket');
                    }

                    const bindResult = __edgebox_socket_bind(this._socketId, port);
                    if (bindResult < 0) {
                        throw new Error(`Failed to bind to port ${port}: ${bindResult}`);
                    }

                    const listenResult = __edgebox_socket_listen(this._socketId, backlog);
                    if (listenResult < 0) {
                        throw new Error(`Failed to listen: ${listenResult}`);
                    }

                    this.listening = true;
                    this._port = port;
                    this._host = host;

                    // Start polling for connections
                    this._acceptPollInterval = setInterval(() => {
                        if (!this.listening || this._socketId === null) {
                            this._stopAcceptPolling();
                            return;
                        }
                        const clientSocketId = __edgebox_socket_accept(this._socketId);
                        if (clientSocketId > 0) {
                            const clientSocket = new Socket({ fd: clientSocketId });
                            clientSocket.remotePort = port;
                            this._connections.add(clientSocket);
                            clientSocket.on('close', () => this._connections.delete(clientSocket));
                            this.emit('connection', clientSocket);
                        }
                    }, 10);

                    this.emit('listening');
                } catch (err) {
                    this.emit('error', err);
                }
            }, 0);

            return this;
        }

        _stopAcceptPolling() {
            if (this._acceptPollInterval) {
                clearInterval(this._acceptPollInterval);
                this._acceptPollInterval = null;
            }
        }

        close(callback) {
            if (callback) this.once('close', callback);

            this._stopAcceptPolling();
            this.listening = false;

            // Close all connections
            for (const conn of this._connections) {
                conn.destroy();
            }
            this._connections.clear();

            if (this._socketId !== null) {
                __edgebox_socket_close(this._socketId);
                this._socketId = null;
            }

            setTimeout(() => this.emit('close'), 0);
            return this;
        }

        address() {
            if (!this.listening) return null;
            return { port: this._port, address: this._host, family: 'IPv4' };
        }

        getConnections(callback) {
            const count = this._connections.size;
            if (callback) setTimeout(() => callback(null, count), 0);
            return this;
        }

        ref() { return this; }
        unref() { return this; }
    }

    _modules.net = {
        Socket,
        Server,
        connect: function(options, callback) {
            const socket = new Socket();
            socket.connect(options, callback);
            return socket;
        },
        createConnection: function(options, callback) {
            return this.connect(options, callback);
        },
        createServer: function(options, connectionListener) {
            return new Server(options, connectionListener);
        },
        isIP: function(input) {
            // IPv4: validate format and octet range (0-255)
            if (/^(\d{1,3}\.){3}\d{1,3}$/.test(input)) {
                var parts = input.split('.');
                if (parts.every(function(p) { return parseInt(p, 10) <= 255; })) {
                    return 4;
                }
            }
            // IPv6: must have at least 2 colons (e.g., "::1", "2001:db8::1")
            // Security: Also validate only hex digits and colons allowed, no triple colons
            if (typeof input === 'string' && input.includes(':') && (input.match(/:/g) || []).length >= 2) {
                // Must only contain hex digits (0-9, a-f, A-F) and colons
                if (/^[0-9a-fA-F:]+$/.test(input) && !/:::/.test(input)) {
                    return 6;
                }
            }
            return 0;
        },
        isIPv4: function(input) { return this.isIP(input) === 4; },
        isIPv6: function(input) { return this.isIP(input) === 6; },
    };
    _modules['node:net'] = _modules.net;

    // ===== TLS MODULE =====
    // Stub tls module - TLS connections not supported in WASM
    class TLSSocket extends Socket {
        constructor(socket, options = {}) {
            super();
            this.authorized = false;
            this.encrypted = true;
            this.alpnProtocol = null;
        }
        getPeerCertificate(detailed) { return {}; }
        getCipher() { return { name: 'TLS_NULL', version: 'TLSv1.3' }; }
        getProtocol() { return 'TLSv1.3'; }
        getSession() { return null; }
        getTLSTicket() { return null; }
        isSessionReused() { return false; }
        setMaxSendFragment(size) { return true; }
        setServername(name) {}
        exportKeyingMaterial(length, label, context) { return Buffer.alloc(0); }
    }

    _modules.tls = {
        TLSSocket,
        connect: function(options, callback) {
            const socket = new TLSSocket();
            setTimeout(() => {
                socket.emit('error', new Error('TLS connections not supported in WASM environment'));
            }, 0);
            if (callback) socket.on('secureConnect', callback);
            return socket;
        },
        createServer: function(options, secureConnectionListener) {
            throw new Error('tls.createServer() not supported in WASM environment');
        },
        createSecureContext: function(options) {
            return { context: {} };
        },
        getCiphers: function() {
            return ['TLS_AES_256_GCM_SHA384', 'TLS_AES_128_GCM_SHA256'];
        },
        DEFAULT_ECDH_CURVE: 'auto',
        DEFAULT_MAX_VERSION: 'TLSv1.3',
        DEFAULT_MIN_VERSION: 'TLSv1.2',
        rootCertificates: [],
    };
    _modules['node:tls'] = _modules.tls;

    // ===== DGRAM MODULE =====
    // UDP sockets implemented via Unix domain sockets (SOCK_DGRAM)
    // For now, using a simpler message-based approach with TCP sockets

    class DgramSocket extends EventEmitter {
        constructor(type, callback) {
            super();
            var self = this;
            this._type = type || 'udp4';
            this._bound = false;
            this._socketId = null;
            this._port = null;
            this._address = '0.0.0.0';

            if (callback) this.on('message', callback);

            // Create socket
            if (typeof __edgebox_socket_create === 'function') {
                this._socketId = __edgebox_socket_create();
                if (this._socketId < 0) {
                    setTimeout(function() {
                        self.emit('error', new Error('Failed to create UDP socket'));
                    }, 0);
                }
            }
        }

        bind(port, address, callback) {
            var self = this;
            if (typeof port === 'object') {
                var options = port;
                port = options.port;
                address = options.address;
                callback = address;
            }
            if (typeof address === 'function') {
                callback = address;
                address = '0.0.0.0';
            }

            this._port = port || 0;
            this._address = address || '0.0.0.0';

            if (callback) this.once('listening', callback);

            setTimeout(function() {
                if (self._socketId !== null && typeof __edgebox_socket_bind === 'function') {
                    var result = __edgebox_socket_bind(self._socketId, self._port);
                    if (result < 0) {
                        self.emit('error', new Error('Failed to bind UDP socket: ' + result));
                        return;
                    }
                }
                self._bound = true;
                self.emit('listening');

                // Start receiving messages
                self._startReceiving();
            }, 0);

            return self;
        }

        _startReceiving() {
            var self = this;
            if (!this._socketId) return;

            this._recvInterval = setInterval(function() {
                if (!self._bound || !self._socketId) {
                    self._stopReceiving();
                    return;
                }
                if (typeof __edgebox_socket_read === 'function') {
                    var data = __edgebox_socket_read(self._socketId, 65536);
                    if (data === null) {
                        // Socket closed
                        self._stopReceiving();
                        self.emit('close');
                        return;
                    }
                    if (data && data.length > 0) {
                        var msg = Buffer.from(data);
                        var rinfo = { address: self._address, family: self._type === 'udp6' ? 'IPv6' : 'IPv4', port: self._port, size: msg.length };
                        self.emit('message', msg, rinfo);
                    }
                }
            }, 10);
        }

        _stopReceiving() {
            if (this._recvInterval) {
                clearInterval(this._recvInterval);
                this._recvInterval = null;
            }
        }

        send(msg, offset, length, port, address, callback) {
            var self = this;

            // Handle different argument patterns
            if (typeof offset === 'number' && typeof length === 'number') {
                // Full signature: msg, offset, length, port, address, callback
            } else if (typeof offset === 'number') {
                // msg, port, address, callback
                callback = address;
                address = length;
                port = offset;
                offset = 0;
                length = msg.length;
            } else if (Array.isArray(msg)) {
                // Array of buffers
                msg = Buffer.concat(msg);
                port = offset;
                address = length;
                callback = port;
                offset = 0;
                length = msg.length;
            }

            if (typeof callback !== 'function') callback = null;

            var buf = Buffer.isBuffer(msg) ? msg : Buffer.from(msg);
            var data = buf.toString().substring(offset, offset + length);

            setTimeout(function() {
                if (self._socketId !== null && typeof __edgebox_socket_write === 'function') {
                    var result = __edgebox_socket_write(self._socketId, data);
                    if (result < 0) {
                        var err = new Error('Send failed: ' + result);
                        if (callback) callback(err);
                        else self.emit('error', err);
                        return;
                    }
                }
                if (callback) callback(null);
            }, 0);
        }

        close(callback) {
            var self = this;
            this._stopReceiving();
            this._bound = false;

            if (this._socketId !== null && typeof __edgebox_socket_close === 'function') {
                __edgebox_socket_close(this._socketId);
                this._socketId = null;
            }

            if (callback) this.once('close', callback);
            setTimeout(function() { self.emit('close'); }, 0);
        }

        address() {
            return { address: this._address, family: this._type === 'udp6' ? 'IPv6' : 'IPv4', port: this._port };
        }

        setBroadcast(flag) { return this; }
        setMulticastTTL(ttl) { return this; }
        setMulticastLoopback(flag) { return this; }
        setTTL(ttl) { return this; }
        addMembership(multicastAddress, multicastInterface) { return this; }
        dropMembership(multicastAddress, multicastInterface) { return this; }
        addSourceSpecificMembership(sourceAddress, groupAddress, multicastInterface) { return this; }
        dropSourceSpecificMembership(sourceAddress, groupAddress, multicastInterface) { return this; }
        setMulticastInterface(multicastInterface) { return this; }
        setRecvBufferSize(size) { return this; }
        setSendBufferSize(size) { return this; }
        getRecvBufferSize() { return 65536; }
        getSendBufferSize() { return 65536; }
        ref() { return this; }
        unref() { return this; }
        remoteAddress() { return undefined; }
        connect(port, address, callback) {
            if (callback) setTimeout(callback, 0);
        }
        disconnect() {}
    }

    _modules.dgram = {
        createSocket: function(type, callback) {
            if (typeof type === 'object') {
                return new DgramSocket(type.type, callback);
            }
            return new DgramSocket(type, callback);
        },
        Socket: DgramSocket
    };
    _modules['node:dgram'] = _modules.dgram;

    // ===== URL MODULE =====
    _modules.url = {
        URL: globalThis.URL,
        URLSearchParams: globalThis.URLSearchParams,
        parse: function(urlStr) {
            try {
                const u = new URL(urlStr);
                return { href: u.href, protocol: u.protocol, host: u.host, hostname: u.hostname, port: u.port, pathname: u.pathname, search: u.search, hash: u.hash, query: Object.fromEntries(u.searchParams) };
            } catch (e) { return { href: urlStr }; }
        },
        format: function(obj) {
            if (typeof obj === 'string') return obj;
            let url = '';
            if (obj.protocol) url += obj.protocol + '//';
            if (obj.hostname || obj.host) url += obj.hostname || obj.host;
            if (obj.port) url += ':' + obj.port;
            if (obj.pathname) url += obj.pathname;
            if (obj.search) url += obj.search;
            if (obj.hash) url += obj.hash;
            return url;
        },
        resolve: (from, to) => new URL(to, from).href,
        // fileURLToPath - convert file:// URL to path
        fileURLToPath: function(urlOrString) {
            let url = typeof urlOrString === 'string' ? urlOrString : urlOrString.href;
            if (!url.startsWith('file://')) {
                throw new TypeError('The URL must be of scheme file');
            }
            // Remove file:// prefix and decode
            let path = decodeURIComponent(url.slice(7));
            // Handle Windows paths (file:///C:/...)
            if (path.match(/^\/[A-Za-z]:\//)) {
                path = path.slice(1);
            }
            return path;
        },
        // pathToFileURL - convert path to file:// URL
        pathToFileURL: function(path) {
            // Ensure absolute path
            if (!path.startsWith('/')) {
                path = '/' + path;
            }
            return new URL('file://' + encodeURIComponent(path).replace(/%2F/g, '/'));
        }
    };

    // ===== OS MODULE =====
    // Spoof as Darwin x64 to avoid "Unsupported architecture: wasm32" errors
    _modules.os = {
        platform: () => 'darwin',
        arch: () => 'x64',
        type: () => 'Darwin',
        release: () => '1.0.0',
        hostname: () => 'edgebox',
        homedir: () => typeof __edgebox_homedir === 'function' ? __edgebox_homedir() : '/home/user',
        tmpdir: () => '/tmp',
        cpus: () => [{ model: 'WASM', speed: 0, times: { user: 0, nice: 0, sys: 0, idle: 0, irq: 0 } }],
        totalmem: () => typeof __edgebox_totalmem === 'function' ? __edgebox_totalmem() : 4294967296,
        freemem: () => typeof __edgebox_freemem === 'function' ? __edgebox_freemem() : 2147483648,
        uptime: () => 0,
        loadavg: () => [0, 0, 0],
        networkInterfaces: () => ({}),
        userInfo: () => ({ username: 'user', uid: 1000, gid: 1000, shell: '/bin/sh', homedir: '/home/user' }),
        endianness: () => 'LE',
        EOL: '\n',
        constants: { signals: {}, errno: {} }
    };

    // ===== CONSTANTS MODULE =====
    // Node.js constants module (fs, os constants)
    _modules.constants = {
        // File access constants
        F_OK: 0,
        R_OK: 4,
        W_OK: 2,
        X_OK: 1,
        // File open constants
        O_RDONLY: 0,
        O_WRONLY: 1,
        O_RDWR: 2,
        O_CREAT: 0o100,
        O_EXCL: 0o200,
        O_NOCTTY: 0o400,
        O_TRUNC: 0o1000,
        O_APPEND: 0o2000,
        O_DIRECTORY: 0o200000,
        O_NOFOLLOW: 0o400000,
        O_SYNC: 0o4010000,
        O_DSYNC: 0o10000,
        O_SYMLINK: 0o40000,
        O_NONBLOCK: 0o4000,
        // File type constants
        S_IFMT: 0o170000,
        S_IFREG: 0o100000,
        S_IFDIR: 0o40000,
        S_IFCHR: 0o20000,
        S_IFBLK: 0o60000,
        S_IFIFO: 0o10000,
        S_IFLNK: 0o120000,
        S_IFSOCK: 0o140000,
        // Permission constants
        S_IRWXU: 0o700,
        S_IRUSR: 0o400,
        S_IWUSR: 0o200,
        S_IXUSR: 0o100,
        S_IRWXG: 0o70,
        S_IRGRP: 0o40,
        S_IWGRP: 0o20,
        S_IXGRP: 0o10,
        S_IRWXO: 0o7,
        S_IROTH: 0o4,
        S_IWOTH: 0o2,
        S_IXOTH: 0o1,
        // UV error codes
        UV_UDP_REUSEADDR: 4,
        COPYFILE_EXCL: 1,
        COPYFILE_FICLONE: 2,
        COPYFILE_FICLONE_FORCE: 4,
    };
    _modules['node:constants'] = _modules.constants;

    // ===== UTIL MODULE =====
    _modules.util = {
        format: function(fmt, ...args) {
            if (typeof fmt !== 'string') return args.length ? [fmt, ...args].map(a => String(a)).join(' ') : '';
            let i = 0;
            return fmt.replace(/%[sdjoO%]/g, match => {
                if (match === '%%') return '%';
                if (i >= args.length) return match;
                const arg = args[i++];
                switch (match) {
                    case '%s': return String(arg);
                    case '%d': return Number(arg).toString();
                    case '%j': case '%o': case '%O':
                        try { return JSON.stringify(arg); } catch { return '[Circular]'; }
                    default: return match;
                }
            });
        },
        inspect: (obj, opts) => JSON.stringify(obj, null, 2),
        promisify: fn => (...args) => new Promise((resolve, reject) => fn(...args, (err, result) => err ? reject(err) : resolve(result))),
        inherits: (ctor, superCtor) => { ctor.super_ = superCtor; Object.setPrototypeOf(ctor.prototype, superCtor.prototype); },
        isArray: Array.isArray,
        isBoolean: v => typeof v === 'boolean',
        isNull: v => v === null,
        isNullOrUndefined: v => v == null,
        isNumber: v => typeof v === 'number',
        isString: v => typeof v === 'string',
        isSymbol: v => typeof v === 'symbol',
        isUndefined: v => v === undefined,
        isRegExp: v => v instanceof RegExp,
        isObject: v => v !== null && typeof v === 'object',
        isDate: v => v instanceof Date,
        isError: v => v instanceof Error,
        isFunction: v => typeof v === 'function',
        isPrimitive: v => v === null || typeof v !== 'object' && typeof v !== 'function',
        types: {
            isPromise: v => v instanceof Promise,
            isAsyncFunction: v => v?.constructor?.name === 'AsyncFunction',
            isGeneratorFunction: v => v?.constructor?.name === 'GeneratorFunction'
        },
        debuglog: () => () => {},
        deprecate: (fn) => fn,
        TextDecoder: globalThis.TextDecoder,
        TextEncoder: globalThis.TextEncoder
    };

    // ===== PROCESS OBJECT =====
    if (!globalThis.process) globalThis.process = {};

    // Create TTY streams for stdout and stderr (defined later, but referenced here)
    // Set isTTY: false for pipe mode (-p) compatibility
    const _stdout = {
        isTTY: false,
        columns: 80,
        rows: 24,
        write: function(data) {
            if (typeof __edgebox_stdout_write === 'function') {
                __edgebox_stdout_write(String(data));
            } else {
                print(String(data).replace(/\n$/, '')); // print adds newline
            }
            return true;
        },
        end: function() {},
        on: function() { return this; },
        once: function() { return this; },
        emit: function() { return false; },
        getWindowSize: function() { return [this.columns, this.rows]; },
        cursorTo: function(x, y) { this.write(y !== undefined ? `\x1b[${y+1};${x+1}H` : `\x1b[${x+1}G`); },
        moveCursor: function(dx, dy) {
            if (dx > 0) this.write(`\x1b[${dx}C`);
            else if (dx < 0) this.write(`\x1b[${-dx}D`);
            if (dy > 0) this.write(`\x1b[${dy}B`);
            else if (dy < 0) this.write(`\x1b[${-dy}A`);
        },
        clearLine: function(dir) { this.write(`\x1b[${dir === -1 ? 1 : dir === 1 ? 0 : 2}K`); },
        clearScreenDown: function() { this.write('\x1b[J'); },
        getColorDepth: function() { return 8; },
        hasColors: function(count) { return (count || 16) <= 256; }
    };

    const _stderr = {
        isTTY: false,
        columns: 80,
        rows: 24,
        write: function(data) {
            if (typeof __edgebox_stderr_write === 'function') {
                __edgebox_stderr_write(String(data));
            } else {
                print('[stderr] ' + String(data).replace(/\n$/, ''));
            }
            return true;
        },
        end: function() {},
        on: function() { return this; },
        once: function() { return this; },
        emit: function() { return false; },
        getWindowSize: function() { return [this.columns, this.rows]; },
        cursorTo: function(x, y) { this.write(y !== undefined ? `\x1b[${y+1};${x+1}H` : `\x1b[${x+1}G`); },
        moveCursor: function(dx, dy) {
            if (dx > 0) this.write(`\x1b[${dx}C`);
            else if (dx < 0) this.write(`\x1b[${-dx}D`);
            if (dy > 0) this.write(`\x1b[${dy}B`);
            else if (dy < 0) this.write(`\x1b[${-dy}A`);
        },
        clearLine: function(dir) { this.write(`\x1b[${dir === -1 ? 1 : dir === 1 ? 0 : 2}K`); },
        clearScreenDown: function() { this.write('\x1b[J'); },
        getColorDepth: function() { return 8; },
        hasColors: function(count) { return (count || 16) <= 256; }
    };

    // Stdin with proper EventEmitter support
    class StdinStream extends Readable {
        constructor() {
            super();
            this.isTTY = typeof __edgebox_isatty === 'function' ? __edgebox_isatty(0) : false;
            this.isRaw = false;
            this.fd = 0;
            this._encoding = null;
            this._paused = true;
            // In WASM/pipe mode, stdin is immediately closed (no interactive input)
            this._closed = !this.isTTY;
            this._endEmitted = false;
        }
        // Override on() to emit 'end' immediately if stdin is already closed
        on(event, listener) {
            super.on(event, listener);
            // If adding 'end' listener on closed stdin, emit 'end' asynchronously
            if ((event === 'end' || event === 'close') && this._closed && !this._endEmitted) {
                print('[STDIN] adding ' + event + ' listener on closed stdin, will emit end');
                this._endEmitted = true;
                setTimeout(() => {
                    print('[STDIN] emitting end/close');
                    this.emit('end');
                    this.emit('close');
                }, 0);
            }
            return this;
        }
        setRawMode(mode) { this.isRaw = mode; return this; }
        setEncoding(enc) { this._encoding = enc; return this; }
        pause() { this._paused = true; return this; }
        resume() {
            this._paused = false;
            // Start reading if we have listeners
            if (this.listenerCount('data') > 0) {
                this._startReading();
            }
            return this;
        }
        _startReading() {
            // If stdin is already closed (non-TTY/pipe mode), emit end immediately
            if (this._closed) {
                setTimeout(() => {
                    this.emit('end');
                    this.emit('close');
                }, 0);
                return;
            }
            if (this._paused) return;
            // Only read if we have active data listeners waiting
            if (this.listenerCount('data') === 0) return;

            // Check if stdin has data ready (non-blocking)
            if (typeof __edgebox_stdin_ready === 'function') {
                if (!__edgebox_stdin_ready()) {
                    // No data ready, retry later (non-blocking poll)
                    if (!this._paused && !this._closed) {
                        setTimeout(() => this._startReading(), 50);
                    }
                    return;
                }
            }

            // Data is ready (or no readiness check available), now safe to read
            if (typeof __edgebox_read_stdin === 'function') {
                const data = __edgebox_read_stdin(4096);
                if (data && data.length > 0) {
                    const buf = Buffer.from(data);
                    this.emit('data', this._encoding ? buf.toString(this._encoding) : buf);
                    // Schedule next read
                    if (!this._paused && !this._closed) {
                        setTimeout(() => this._startReading(), 0);
                    }
                } else if (data === null) {
                    this._closed = true;
                    this.emit('end');
                    this.emit('close');
                }
            }
        }
        read(size) {
            if (typeof __edgebox_read_stdin === 'function') {
                const data = __edgebox_read_stdin(size || 4096);
                return data ? Buffer.from(data) : null;
            }
            return null;
        }
    }
    const _stdin = new StdinStream();

    Object.assign(globalThis.process, {
        env: (globalThis.process && globalThis.process.env) || {},
        argv: (globalThis.process && globalThis.process.argv) || [],
        cwd: () => typeof __edgebox_cwd === 'function' ? __edgebox_cwd() : '/',
        version: 'v18.0.0-edgebox',
        versions: { node: '18.0.0', v8: '0.0.0', quickjs: '2024.1' },
        platform: 'darwin',
        arch: 'x64',
        hrtime: { bigint: () => BigInt(Date.now()) * 1000000n },
        nextTick: (fn, ...args) => { setTimeout(() => fn(...args), 0); },
        stdout: _stdout,
        stderr: _stderr,
        stdin: _stdin,
        exit: (code) => { throw new Error('process.exit(' + code + ')'); },
        on: function() { return this; },
        once: function() { return this; },
        emit: function() { return false; },
        removeListener: function() { return this; },
        listeners: function() { return []; }
    });

    // ===== ADDITIONAL MODULES =====
    // Note: net module is defined above with full Socket/Server implementation

    // Zlib module with native compression support
    _modules.zlib = {
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

        // Stream creators
        createGzip: function(options) {
            const transform = new Transform();
            transform._transform = (chunk, encoding, callback) => {
                try {
                    callback(null, _modules.zlib.gzipSync(chunk));
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
                    callback(null, _modules.zlib.gunzipSync(chunk));
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
                    callback(null, _modules.zlib.deflateSync(chunk));
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
                    callback(null, _modules.zlib.inflateSync(chunk));
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
                    callback(null, _modules.zlib.gunzipSync(buf));
                } else {
                    callback(null, _modules.zlib.inflateSync(buf));
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
    _modules['node:zlib'] = _modules.zlib;

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

    // Buffer module
    _modules.buffer = { Buffer };

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

    // DNS module - limited in WASM, provides basic stubs
    _modules.dns = {
        lookup: function(hostname, options, callback) {
            if (typeof options === 'function') { callback = options; options = {}; }
            // Can't do real DNS in WASM, return localhost for loopback or error for others
            if (hostname === 'localhost' || hostname === '127.0.0.1' || hostname === '::1') {
                setTimeout(() => callback(null, '127.0.0.1', 4), 0);
            } else {
                // For other hostnames, still return a placeholder but warn
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
        // Constants
        ADDRCONFIG: 1024,
        V4MAPPED: 2048,
        NODATA: 'ENODATA',
        FORMERR: 'EFORMERR',
        SERVFAIL: 'ESERVFAIL',
        NOTFOUND: 'ENOTFOUND',
        NOTIMP: 'ENOTIMP',
        REFUSED: 'EREFUSED',
        BADQUERY: 'EBADQUERY',
        BADNAME: 'EBADNAME',
        BADFAMILY: 'EBADFAMILY',
        BADRESP: 'EBADRESP',
        CONNREFUSED: 'ECONNREFUSED',
        TIMEOUT: 'ETIMEOUT',
        EOF: 'EOF',
        FILE: 'EFILE',
        NOMEM: 'ENOMEM',
        DESTRUCTION: 'EDESTRUCTION',
        BADSTR: 'EBADSTR',
        BADFLAGS: 'EBADFLAGS',
        NONAME: 'ENONAME',
        BADHINTS: 'EBADHINTS',
        NOTINITIALIZED: 'ENOTINITIALIZED',
        LOADIPHLPAPI: 'ELOADIPHLPAPI',
        ADDRGETNETWORKPARAMS: 'EADDRGETNETWORKPARAMS',
        CANCELLED: 'ECANCELLED'
    };
    _modules['node:dns'] = _modules.dns;

    // Inspector module - debugging not available in WASM
    _modules.inspector = {
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
    _modules['node:inspector'] = _modules.inspector;

    // V8 module - V8-specific APIs not available in QuickJS
    _modules.v8 = {
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
    _modules['node:v8'] = _modules.v8;

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
    _modules.querystring = {
        parse: str => Object.fromEntries(new URLSearchParams(str)),
        stringify: obj => new URLSearchParams(obj).toString(),
        escape: encodeURIComponent,
        unescape: decodeURIComponent
    };

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

    _modules.worker_threads = {
        isMainThread: !_isWorkerProcess,
        parentPort: _parentPort,
        workerData: _isWorkerProcess ? (function() {
            try { return JSON.parse(process.env.__EDGEBOX_WORKER_DATA || 'null'); } catch(e) { return null; }
        })() : null,
        threadId: _isWorkerProcess ? parseInt(process.env.__EDGEBOX_WORKER_ID || '0', 10) : 0,
        Worker: Worker,
        MessageChannel: MessageChannel,
        MessagePort: MessagePort,
        BroadcastChannel: class BroadcastChannel extends EventEmitter {
            constructor(name) { super(); this.name = name; }
            postMessage() {}
            close() {}
        },
        // Additional exports
        SHARE_ENV: Symbol('SHARE_ENV'),
        setEnvironmentData: function(key, value) { process.env[key] = value; },
        getEnvironmentData: function(key) { return process.env[key]; },
        markAsUntransferable: function(obj) { return obj; },
        moveMessagePortToContext: function(port) { return port; },
        receiveMessageOnPort: function(port) { return undefined; },
        resourceLimits: {}
    };
    _modules['node:worker_threads'] = _modules.worker_threads;

    // Cluster module - implemented via subprocess forking
    var _clusterWorkerId = 0;
    var _clusterWorkers = {};
    var _isClusterWorker = typeof process.env.CLUSTER_WORKER_ID !== 'undefined';
    var _clusterSettings = { exec: null, args: [], silent: false };

    class ClusterWorker extends EventEmitter {
        constructor(id, child) {
            super();
            this.id = id;
            this.process = child;
            this.exitedAfterDisconnect = false;
            this.state = 'online';
            this.isDead = function() { return this.state === 'dead'; };
            this.isConnected = function() { return this.state === 'online'; };
        }
        send(message, sendHandle, options, callback) {
            if (typeof options === 'function') { callback = options; options = undefined; }
            if (typeof sendHandle === 'function') { callback = sendHandle; sendHandle = undefined; }
            var msg = '__CLUSTER_MSG__:' + JSON.stringify(message) + '\n';
            if (this.process && this.process.stdin) {
                this.process.stdin.write(msg);
            }
            if (callback) setTimeout(callback, 0);
            return true;
        }
        kill(signal) {
            this.state = 'dead';
            if (this.process && this.process.kill) {
                this.process.kill(signal);
            }
        }
        disconnect() {
            this.exitedAfterDisconnect = true;
            this.state = 'disconnected';
            this.emit('disconnect');
        }
    }

    _modules.cluster = Object.assign(new EventEmitter(), {
        isMaster: !_isClusterWorker,
        isPrimary: !_isClusterWorker,
        isWorker: _isClusterWorker,
        workers: _clusterWorkers,
        worker: null,
        settings: _clusterSettings,
        SCHED_NONE: 1,
        SCHED_RR: 2,
        schedulingPolicy: 2,

        setupPrimary: function(settings) {
            if (settings) Object.assign(_clusterSettings, settings);
        },
        setupMaster: function(settings) { this.setupPrimary(settings); },

        fork: function(env) {
            var self = this;
            var id = ++_clusterWorkerId;
            var script = _clusterSettings.exec || process.argv[1];
            var args = _clusterSettings.args || [];

            var workerEnv = Object.assign({}, process.env, env || {});
            workerEnv.CLUSTER_WORKER_ID = String(id);

            var cp = _modules.child_process;
            var child = cp.spawn(process.argv[0], [script].concat(args), {
                env: workerEnv,
                stdio: _clusterSettings.silent ? 'pipe' : 'inherit'
            });

            var worker = new ClusterWorker(id, child);
            _clusterWorkers[id] = worker;

            child.on('exit', function(code, signal) {
                worker.state = 'dead';
                worker.emit('exit', code, signal);
                self.emit('exit', worker, code, signal);
                delete _clusterWorkers[id];
            });

            child.on('error', function(err) {
                worker.emit('error', err);
            });

            if (!_clusterSettings.silent && child.stdout) {
                child.stdout.on('data', function(data) {
                    var lines = data.toString().split('\n');
                    for (var i = 0; i < lines.length; i++) {
                        var line = lines[i].trim();
                        if (line.startsWith('__CLUSTER_MSG__:')) {
                            try {
                                var msg = JSON.parse(line.slice(16));
                                worker.emit('message', msg);
                                self.emit('message', worker, msg);
                            } catch (e) {}
                        }
                    }
                });
            }

            setTimeout(function() {
                worker.emit('online');
                self.emit('online', worker);
            }, 0);

            return worker;
        },

        disconnect: function(callback) {
            var workers = Object.values(_clusterWorkers);
            var pending = workers.length;
            if (pending === 0 && callback) return setTimeout(callback, 0);
            workers.forEach(function(worker) {
                worker.disconnect();
                worker.once('disconnect', function() {
                    pending--;
                    if (pending === 0 && callback) callback();
                });
            });
        }
    });

    // Set up worker-side if in cluster worker
    if (_isClusterWorker) {
        var clusterWorker = {
            id: parseInt(process.env.CLUSTER_WORKER_ID, 10),
            send: function(message, sendHandle, options, callback) {
                if (typeof options === 'function') { callback = options; options = undefined; }
                if (typeof sendHandle === 'function') { callback = sendHandle; sendHandle = undefined; }
                print('__CLUSTER_MSG__:' + JSON.stringify(message));
                if (callback) setTimeout(callback, 0);
                return true;
            },
            disconnect: function() { process.exit(0); },
            kill: function(signal) { process.exit(1); },
            isDead: function() { return false; },
            isConnected: function() { return true; }
        };
        _modules.cluster.worker = clusterWorker;
    }
    _modules['node:cluster'] = _modules.cluster;

    // Diagnostics channel module
    class DiagnosticsChannel {
        constructor(name) { this.name = name; this._subscribers = []; }
        subscribe(fn) { this._subscribers.push(fn); }
        unsubscribe(fn) { this._subscribers = this._subscribers.filter(s => s !== fn); }
        publish(message) { this._subscribers.forEach(fn => fn(message, this.name)); }
        get hasSubscribers() { return this._subscribers.length > 0; }
    }
    const _channels = new Map();
    _modules.diagnostics_channel = {
        channel: (name) => {
            if (!_channels.has(name)) _channels.set(name, new DiagnosticsChannel(name));
            return _channels.get(name);
        },
        hasSubscribers: (name) => _channels.has(name) && _channels.get(name).hasSubscribers,
        subscribe: (name, fn) => _modules.diagnostics_channel.channel(name).subscribe(fn),
        unsubscribe: (name, fn) => { if (_channels.has(name)) _channels.get(name).unsubscribe(fn); },
        Channel: DiagnosticsChannel
    };

    // Punycode module (deprecated but still used)
    _modules.punycode = {
        encode: (s) => s,
        decode: (s) => s,
        toASCII: (s) => s,
        toUnicode: (s) => s
    };

    // Console module (alias to global console)
    _modules.console = globalThis.console || {
        log: print,
        error: print,
        warn: print,
        info: print,
        debug: print,
        trace: print,
        dir: (obj) => print(JSON.stringify(obj, null, 2)),
        time: () => {},
        timeEnd: () => {},
        assert: (cond, msg) => { if (!cond) throw new Error(msg); }
    };

    // node-fetch module (uses our global fetch)
    // Claude Code uses node-fetch for HTTP requests to Anthropic API
    const nodeFetch = globalThis.fetch || function() { throw new Error('fetch not available'); };
    nodeFetch.default = nodeFetch;
    nodeFetch.Headers = globalThis.Headers || function(init) { this._headers = init || {}; };
    nodeFetch.Request = globalThis.Request || function(url, opts) { this.url = url; this.method = opts?.method || 'GET'; };
    nodeFetch.Response = globalThis.Response || function(body, opts) { this.body = body; this.status = opts?.status || 200; };
    _modules['node-fetch'] = nodeFetch;

    // ===== REQUIRE FUNCTION =====
    globalThis.require = function(name) {
        // Strip node: prefix
        let moduleName = name.startsWith('node:') ? name.slice(5) : name;

        // Direct lookup
        if (_modules[moduleName]) {
            return _modules[moduleName];
        }

        // Handle subpaths like path/win32 -> path
        const baseName = moduleName.split('/')[0];
        if (_modules[baseName]) return _modules[baseName];

        // Debug logging when enabled
        if (globalThis._edgebox_debug) {
            print('[EDGEBOX JS] Module not found: ' + name + ' (normalized: ' + moduleName + ')');
            print('[EDGEBOX JS] Available modules: ' + Object.keys(_modules).join(', '));
        }

        throw new Error('Module not found: ' + name);
    };
    globalThis.require.resolve = name => name;
    globalThis.require.cache = {};

    // createRequire function for ES modules
    _modules.module.createRequire = () => globalThis.require;

    // ===== EXPOSE GLOBAL WEB APIs =====
    // Timer functions - always polyfill if not defined
    if (typeof globalThis.setTimeout === 'undefined') {
        // Try to use QuickJS os.setTimeout for proper event loop integration
        const _os = (typeof globalThis._os !== 'undefined' && globalThis._os)
                  || (typeof os !== 'undefined' ? os : null);
        const _timers = new Map();
        let _timerId = 0;

        if (_os && typeof _os.setTimeout === 'function') {
            // Use QuickJS native timers
            globalThis.setTimeout = function(fn, delay, ...args) {
                const id = ++_timerId;
                // Debug: log timers with delay > 10 seconds
                if ((delay || 0) > 10000) {
                    _log('[setTimeout] #' + id + ' delay=' + (delay || 0) + 'ms (' + Math.round((delay||0)/1000) + 's)');
                }
                const handle = _os.setTimeout(() => {
                    _timers.delete(id);
                    fn(...args);
                }, delay || 0);
                _timers.set(id, handle);
                return id;
            };

            globalThis.clearTimeout = function(id) {
                const handle = _timers.get(id);
                if (handle !== undefined) {
                    _os.clearTimeout(handle);
                    _timers.delete(id);
                }
            };

            globalThis.setInterval = function(fn, delay, ...args) {
                const id = ++_timerId;
                const tick = () => {
                    fn(...args);
                    if (_timers.has(id)) {
                        const handle = _os.setTimeout(tick, delay || 0);
                        _timers.set(id, handle);
                    }
                };
                const handle = _os.setTimeout(tick, delay || 0);
                _timers.set(id, handle);
                return id;
            };

            globalThis.clearInterval = function(id) {
                const handle = _timers.get(id);
                if (handle !== undefined) {
                    _os.clearTimeout(handle);
                    _timers.delete(id);
                }
            };
        } else {
            // Fallback - use microtasks for zero-delay, noop for others
            globalThis.setTimeout = function(fn, delay, ...args) {
                const id = ++_timerId;
                _timers.set(id, { fn, args, cleared: false });
                if (delay === 0 || delay === undefined) {
                    queueMicrotask(() => {
                        const timer = _timers.get(id);
                        if (timer && !timer.cleared) {
                            _timers.delete(id);
                            timer.fn(...timer.args);
                        }
                    });
                }
                return id;
            };

            globalThis.clearTimeout = function(id) {
                const timer = _timers.get(id);
                if (timer) timer.cleared = true;
                _timers.delete(id);
            };

            globalThis.setInterval = function(fn, delay, ...args) {
                return ++_timerId; // noop in fallback mode
            };

            globalThis.clearInterval = globalThis.clearTimeout;
        }
    }

    // setImmediate
    if (typeof globalThis.setImmediate === 'undefined') {
        globalThis.setImmediate = function(fn, ...args) {
            return globalThis.setTimeout(fn, 0, ...args);
        };
        globalThis.clearImmediate = globalThis.clearTimeout;
    }

    // TextEncoder/TextDecoder - should be provided by QuickJS or runtime
    if (typeof globalThis.TextEncoder === 'undefined') {
        globalThis.TextEncoder = class TextEncoder {
            constructor(encoding = 'utf-8') { this.encoding = encoding; }
            encode(str) {
                // Simple UTF-8 encoding
                const bytes = [];
                for (let i = 0; i < str.length; i++) {
                    let c = str.charCodeAt(i);
                    if (c < 0x80) {
                        bytes.push(c);
                    } else if (c < 0x800) {
                        bytes.push(0xc0 | (c >> 6), 0x80 | (c & 0x3f));
                    } else if (c < 0x10000) {
                        bytes.push(0xe0 | (c >> 12), 0x80 | ((c >> 6) & 0x3f), 0x80 | (c & 0x3f));
                    } else {
                        bytes.push(0xf0 | (c >> 18), 0x80 | ((c >> 12) & 0x3f), 0x80 | ((c >> 6) & 0x3f), 0x80 | (c & 0x3f));
                    }
                }
                return new Uint8Array(bytes);
            }
            encodeInto(str, u8arr) {
                const encoded = this.encode(str);
                const len = Math.min(encoded.length, u8arr.length);
                u8arr.set(encoded.slice(0, len));
                return { read: str.length, written: len };
            }
        };
    }

    if (typeof globalThis.TextDecoder === 'undefined') {
        globalThis.TextDecoder = class TextDecoder {
            constructor(encoding = 'utf-8') { this.encoding = encoding; this.fatal = false; }
            decode(input) {
                if (!input) return '';
                const bytes = input instanceof Uint8Array ? input : new Uint8Array(input);
                let result = '';
                let i = 0;
                while (i < bytes.length) {
                    const b = bytes[i];
                    if (b < 0x80) {
                        result += String.fromCharCode(b);
                        i++;
                    } else if ((b & 0xe0) === 0xc0) {
                        result += String.fromCharCode(((b & 0x1f) << 6) | (bytes[i + 1] & 0x3f));
                        i += 2;
                    } else if ((b & 0xf0) === 0xe0) {
                        result += String.fromCharCode(((b & 0x0f) << 12) | ((bytes[i + 1] & 0x3f) << 6) | (bytes[i + 2] & 0x3f));
                        i += 3;
                    } else {
                        const codePoint = ((b & 0x07) << 18) | ((bytes[i + 1] & 0x3f) << 12) | ((bytes[i + 2] & 0x3f) << 6) | (bytes[i + 3] & 0x3f);
                        if (codePoint > 0x10000) {
                            const offset = codePoint - 0x10000;
                            result += String.fromCharCode(0xd800 + (offset >> 10), 0xdc00 + (offset & 0x3ff));
                        } else {
                            result += String.fromCharCode(codePoint);
                        }
                        i += 4;
                    }
                }
                return result;
            }
        };
    }

    // URL and URLSearchParams
    if (typeof globalThis.URL === 'undefined') {
        globalThis.URLSearchParams = class URLSearchParams {
            constructor(init = '') {
                // Security: Limit max parameters to prevent DoS
                const MAX_PARAMS = 1000;
                this._params = new Map();
                if (typeof init === 'string') {
                    init = init.startsWith('?') ? init.slice(1) : init;
                    const pairs = init.split('&');
                    const limit = Math.min(pairs.length, MAX_PARAMS);
                    for (let i = 0; i < limit; i++) {
                        const pair = pairs[i];
                        const [key, value = ''] = pair.split('=').map(decodeURIComponent);
                        if (key) this.append(key, value);
                    }
                } else if (init instanceof URLSearchParams) {
                    init.forEach((v, k) => this.append(k, v));
                } else if (typeof init === 'object') {
                    for (const [k, v] of Object.entries(init)) this.append(k, v);
                }
            }
            append(name, value) {
                if (!this._params.has(name)) this._params.set(name, []);
                this._params.get(name).push(String(value));
            }
            delete(name) { this._params.delete(name); }
            get(name) { const vals = this._params.get(name); return vals ? vals[0] : null; }
            getAll(name) { return this._params.get(name) || []; }
            has(name) { return this._params.has(name); }
            set(name, value) { this._params.set(name, [String(value)]); }
            toString() {
                const parts = [];
                this._params.forEach((vals, key) => vals.forEach(v => parts.push(encodeURIComponent(key) + '=' + encodeURIComponent(v))));
                return parts.join('&');
            }
            forEach(cb) { this._params.forEach((vals, key) => vals.forEach(v => cb(v, key, this))); }
            *entries() { for (const [k, vals] of this._params) for (const v of vals) yield [k, v]; }
            *keys() { for (const [k, vals] of this._params) for (const _ of vals) yield k; }
            *values() { for (const [k, vals] of this._params) for (const v of vals) yield v; }
            [Symbol.iterator]() { return this.entries(); }
        };

        globalThis.URL = class URL {
            constructor(url, base) {
                if (base) {
                    const baseUrl = typeof base === 'string' ? base : base.href;
                    if (!url.match(/^[a-z]+:/i)) {
                        url = baseUrl.replace(/[^/]*$/, '') + url;
                    }
                }
                // Security: Stricter URL validation
                // - Protocol: must start with letter, can contain letters, digits, +, -, .
                // - Hostname: must have at least one character (not empty), can't be just colons
                const match = url.match(/^([a-z][a-z0-9+\-.]*):\/\/([^/:]+)(?::(\d+))?(\/[^?#]*)?(\?[^#]*)?(#.*)?$/i);
                if (!match) throw new TypeError('Invalid URL: ' + url);
                // Additional validation: hostname must not be empty or only colons
                const hostname = match[2] || '';
                if (!hostname || /^:+$/.test(hostname)) {
                    throw new TypeError('Invalid URL: ' + url);
                }
                this.protocol = match[1].toLowerCase() + ':';
                this.hostname = hostname;
                this.port = match[3] || '';
                this.pathname = match[4] || '/';
                this.search = match[5] || '';
                this.hash = match[6] || '';
                this.searchParams = new URLSearchParams(this.search);
            }
            get host() { return this.port ? this.hostname + ':' + this.port : this.hostname; }
            get origin() { return this.protocol + '//' + this.host; }
            get href() { return this.origin + this.pathname + this.search + this.hash; }
            toString() { return this.href; }
            toJSON() { return this.href; }
        };
    }

    // AbortController/AbortSignal
    if (typeof globalThis.AbortController === 'undefined') {
        globalThis.AbortSignal = class AbortSignal extends EventEmitter {
            constructor() {
                super();
                this.aborted = false;
                this.reason = undefined;
            }
            // DOM-style addEventListener (alias for on())
            addEventListener(event, listener, options) {
                if (options && options.once) {
                    this.once(event, listener);
                } else {
                    this.on(event, listener);
                }
            }
            // DOM-style removeEventListener (alias for off())
            removeEventListener(event, listener) {
                this.off(event, listener);
            }
            throwIfAborted() {
                if (this.aborted) throw this.reason;
            }
            static abort(reason) {
                const signal = new AbortSignal();
                signal.aborted = true;
                signal.reason = reason || new DOMException('The operation was aborted.', 'AbortError');
                return signal;
            }
            static timeout(ms) {
                const signal = new AbortSignal();
                setTimeout(() => {
                    signal.aborted = true;
                    signal.reason = new DOMException('The operation timed out.', 'TimeoutError');
                    signal.emit('abort', signal.reason);
                }, ms);
                return signal;
            }
        };

        globalThis.AbortController = class AbortController {
            constructor() {
                this.signal = new AbortSignal();
            }
            abort(reason) {
                if (!this.signal.aborted) {
                    this.signal.aborted = true;
                    this.signal.reason = reason || new DOMException('The operation was aborted.', 'AbortError');
                    this.signal.emit('abort', this.signal.reason);
                }
            }
        };

        // DOMException polyfill
        if (typeof globalThis.DOMException === 'undefined') {
            globalThis.DOMException = class DOMException extends Error {
                constructor(message, name = 'Error') {
                    super(message);
                    this.name = name;
                }
            };
        }
    }

    // Define v9 globally to match SDK's AbortController creation pattern
    // The SDK uses v9() extensively for creating abort controllers
    if (typeof globalThis.v9 === 'undefined') {
        globalThis.v9 = function(maxListeners = 100) {
            const controller = new AbortController();
            // Call setMaxListeners for compatibility (even though it's a no-op in our polyfill)
            if (_modules.events && _modules.events.setMaxListeners) {
                try {
                    _modules.events.setMaxListeners(maxListeners, controller.signal);
                } catch (e) {
                    // Ignore errors from setMaxListeners
                }
            }
            return controller;
        };
    }

    // Web Crypto API with SubtleCrypto implementation
    if (typeof globalThis.crypto === 'undefined') {
        // CryptoKey class for Web Crypto API
        class CryptoKey {
            constructor(type, extractable, algorithm, usages, keyData) {
                this.type = type; // 'secret', 'public', 'private'
                this.extractable = extractable;
                this.algorithm = algorithm;
                this.usages = usages;
                this._keyData = keyData; // Internal key material
            }
        }

        // Security: Whitelist of valid crypto algorithms
        const VALID_HASH_ALGOS = ['sha1', 'sha256', 'sha384', 'sha512', 'md5'];
        const VALID_SIGN_ALGOS = ['HMAC', 'hmac'];
        const VALID_ENCRYPT_ALGOS = ['AES-GCM', 'aes-gcm'];
        const VALID_DERIVE_ALGOS = ['PBKDF2', 'pbkdf2'];

        // SubtleCrypto implementation
        const subtle = {
            // digest(algorithm, data) - Hash data
            async digest(algorithm, data) {
                const algoName = typeof algorithm === 'string' ? algorithm : algorithm.name;
                const normalizedAlgo = algoName.toLowerCase().replace('-', '');
                // Security: Validate algorithm is in whitelist
                if (!VALID_HASH_ALGOS.includes(normalizedAlgo)) {
                    throw new Error('Unsupported hash algorithm: ' + algoName);
                }

                // Convert data to Uint8Array
                let dataBytes;
                if (data instanceof ArrayBuffer) {
                    dataBytes = new Uint8Array(data);
                } else if (ArrayBuffer.isView(data)) {
                    dataBytes = new Uint8Array(data.buffer, data.byteOffset, data.byteLength);
                } else {
                    throw new TypeError('data must be BufferSource');
                }

                // Use native hash if available
                if (typeof globalThis.__edgebox_hash === 'function') {
                    const dataStr = String.fromCharCode.apply(null, dataBytes);
                    const hexResult = globalThis.__edgebox_hash(normalizedAlgo, dataStr);
                    // Convert hex to ArrayBuffer
                    const bytes = new Uint8Array(hexResult.length / 2);
                    for (let i = 0; i < bytes.length; i++) {
                        bytes[i] = parseInt(hexResult.substring(i * 2, i * 2 + 2), 16);
                    }
                    return bytes.buffer;
                }

                throw new Error('Hash not available');
            },

            // sign(algorithm, key, data) - Create signature
            async sign(algorithm, key, data) {
                if (!(key instanceof CryptoKey)) {
                    throw new TypeError('key must be a CryptoKey');
                }
                if (!key.usages.includes('sign')) {
                    throw new DOMException('Key does not support signing', 'InvalidAccessError');
                }

                const algoName = typeof algorithm === 'string' ? algorithm : algorithm.name;

                let dataBytes;
                if (data instanceof ArrayBuffer) {
                    dataBytes = new Uint8Array(data);
                } else if (ArrayBuffer.isView(data)) {
                    dataBytes = new Uint8Array(data.buffer, data.byteOffset, data.byteLength);
                } else {
                    throw new TypeError('data must be BufferSource');
                }

                if (algoName === 'HMAC') {
                    const hashAlgo = (algorithm.hash || key.algorithm.hash || 'SHA-256');
                    const hashName = (typeof hashAlgo === 'string' ? hashAlgo : hashAlgo.name).toLowerCase().replace('-', '');

                    if (typeof globalThis.__edgebox_hmac === 'function') {
                        const keyStr = String.fromCharCode.apply(null, new Uint8Array(key._keyData));
                        const dataStr = String.fromCharCode.apply(null, dataBytes);
                        const hexResult = globalThis.__edgebox_hmac(hashName, keyStr, dataStr);
                        const bytes = new Uint8Array(hexResult.length / 2);
                        for (let i = 0; i < bytes.length; i++) {
                            bytes[i] = parseInt(hexResult.substring(i * 2, i * 2 + 2), 16);
                        }
                        return bytes.buffer;
                    }
                }

                throw new Error('Sign algorithm not supported: ' + algoName);
            },

            // verify(algorithm, key, signature, data) - Verify signature
            async verify(algorithm, key, signature, data) {
                if (!(key instanceof CryptoKey)) {
                    throw new TypeError('key must be a CryptoKey');
                }
                if (!key.usages.includes('verify')) {
                    throw new DOMException('Key does not support verification', 'InvalidAccessError');
                }

                // Compute expected signature
                const expectedSig = await this.sign(
                    { ...algorithm, name: algorithm.name || 'HMAC' },
                    { ...key, usages: ['sign'] },
                    data
                );

                let sigBytes;
                if (signature instanceof ArrayBuffer) {
                    sigBytes = new Uint8Array(signature);
                } else if (ArrayBuffer.isView(signature)) {
                    sigBytes = new Uint8Array(signature.buffer, signature.byteOffset, signature.byteLength);
                }

                const expectedBytes = new Uint8Array(expectedSig);

                // Constant-time comparison
                if (sigBytes.length !== expectedBytes.length) return false;
                let result = 0;
                for (let i = 0; i < sigBytes.length; i++) {
                    result |= sigBytes[i] ^ expectedBytes[i];
                }
                return result === 0;
            },

            // generateKey(algorithm, extractable, keyUsages) - Generate key
            async generateKey(algorithm, extractable, keyUsages) {
                const algoName = typeof algorithm === 'string' ? algorithm : algorithm.name;

                if (algoName === 'HMAC') {
                    const hashAlgo = algorithm.hash || 'SHA-256';
                    const hashName = typeof hashAlgo === 'string' ? hashAlgo : hashAlgo.name;
                    const length = algorithm.length || (hashName === 'SHA-512' ? 512 : hashName === 'SHA-384' ? 384 : 256);

                    const keyData = new Uint8Array(length / 8);
                    globalThis.crypto.getRandomValues(keyData);

                    return new CryptoKey('secret', extractable, { name: 'HMAC', hash: hashAlgo, length }, keyUsages, keyData.buffer);
                }

                if (algoName === 'AES-GCM' || algoName === 'AES-CBC' || algoName === 'AES-CTR') {
                    const length = algorithm.length || 256;
                    const keyData = new Uint8Array(length / 8);
                    globalThis.crypto.getRandomValues(keyData);

                    return new CryptoKey('secret', extractable, { name: algoName, length }, keyUsages, keyData.buffer);
                }

                throw new Error('Key generation not supported for: ' + algoName);
            },

            // importKey(format, keyData, algorithm, extractable, keyUsages) - Import key
            async importKey(format, keyData, algorithm, extractable, keyUsages) {
                const algoName = typeof algorithm === 'string' ? algorithm : algorithm.name;

                let rawKeyData;
                if (format === 'raw') {
                    if (keyData instanceof ArrayBuffer) {
                        rawKeyData = keyData;
                    } else if (ArrayBuffer.isView(keyData)) {
                        rawKeyData = keyData.buffer.slice(keyData.byteOffset, keyData.byteOffset + keyData.byteLength);
                    } else {
                        throw new TypeError('keyData must be BufferSource for raw format');
                    }
                } else if (format === 'jwk') {
                    // JWK import - Security: Validate JWK structure
                    if (typeof keyData !== 'object' || keyData === null) {
                        throw new TypeError('JWK must be an object');
                    }
                    // Validate kty field for symmetric keys
                    if (keyData.kty && keyData.kty !== 'oct') {
                        throw new Error('Only symmetric keys (kty: "oct") are supported');
                    }
                    if (keyData.k) {
                        // Security: Validate keyData.k is a string before processing
                        if (typeof keyData.k !== 'string') {
                            throw new TypeError('JWK "k" parameter must be a string');
                        }
                        // Security: Validate k is valid base64url (alphanumeric, -, _)
                        if (!/^[A-Za-z0-9_-]*$/.test(keyData.k)) {
                            throw new TypeError('JWK "k" contains invalid characters');
                        }
                        // Base64url decode
                        const base64 = keyData.k.replace(/-/g, '+').replace(/_/g, '/');
                        const padding = '='.repeat((4 - base64.length % 4) % 4);
                        const binary = atob(base64 + padding);
                        rawKeyData = new Uint8Array(binary.length);
                        for (let i = 0; i < binary.length; i++) {
                            rawKeyData[i] = binary.charCodeAt(i);
                        }
                        rawKeyData = rawKeyData.buffer;
                    } else {
                        throw new Error('JWK must contain "k" for symmetric keys');
                    }
                } else {
                    throw new Error('Unsupported key format: ' + format);
                }

                if (algoName === 'HMAC') {
                    return new CryptoKey('secret', extractable, { name: 'HMAC', hash: algorithm.hash || 'SHA-256' }, keyUsages, rawKeyData);
                }

                if (algoName === 'AES-GCM' || algoName === 'AES-CBC' || algoName === 'AES-CTR') {
                    return new CryptoKey('secret', extractable, { name: algoName, length: rawKeyData.byteLength * 8 }, keyUsages, rawKeyData);
                }

                if (algoName === 'PBKDF2') {
                    return new CryptoKey('secret', false, { name: 'PBKDF2' }, keyUsages, rawKeyData);
                }

                throw new Error('Import not supported for: ' + algoName);
            },

            // exportKey(format, key) - Export key
            async exportKey(format, key) {
                if (!(key instanceof CryptoKey)) {
                    throw new TypeError('key must be a CryptoKey');
                }
                if (!key.extractable) {
                    throw new DOMException('Key is not extractable', 'InvalidAccessError');
                }

                if (format === 'raw') {
                    return key._keyData;
                }

                if (format === 'jwk') {
                    const keyBytes = new Uint8Array(key._keyData);
                    // Base64url encode
                    let binary = '';
                    for (let i = 0; i < keyBytes.length; i++) {
                        binary += String.fromCharCode(keyBytes[i]);
                    }
                    const base64 = btoa(binary).replace(/\+/g, '-').replace(/\//g, '_').replace(/=/g, '');

                    return {
                        kty: 'oct',
                        k: base64,
                        alg: key.algorithm.name === 'HMAC' ? 'HS256' : 'A256GCM',
                        ext: key.extractable,
                        key_ops: key.usages
                    };
                }

                throw new Error('Unsupported export format: ' + format);
            },

            // deriveBits(algorithm, baseKey, length) - Derive bits from key
            async deriveBits(algorithm, baseKey, length) {
                if (!(baseKey instanceof CryptoKey)) {
                    throw new TypeError('baseKey must be a CryptoKey');
                }

                const algoName = typeof algorithm === 'string' ? algorithm : algorithm.name;

                if (algoName === 'PBKDF2') {
                    // Simple PBKDF2 implementation using HMAC
                    const salt = algorithm.salt instanceof ArrayBuffer
                        ? new Uint8Array(algorithm.salt)
                        : new Uint8Array(algorithm.salt.buffer, algorithm.salt.byteOffset, algorithm.salt.byteLength);
                    // Security: Limit iterations to prevent DoS (max 10 million)
                    const MAX_ITERATIONS = 10000000;
                    const iterations = Math.min(Math.max(algorithm.iterations || 1000, 1), MAX_ITERATIONS);
                    const hashAlgo = (algorithm.hash || 'SHA-256');
                    const hashName = (typeof hashAlgo === 'string' ? hashAlgo : hashAlgo.name).toLowerCase().replace('-', '');

                    if (typeof globalThis.__edgebox_hmac === 'function') {
                        const password = new Uint8Array(baseKey._keyData);
                        const dkLen = length / 8;
                        const hLen = hashName === 'sha512' ? 64 : hashName === 'sha384' ? 48 : 32;
                        const numBlocks = Math.ceil(dkLen / hLen);
                        const result = new Uint8Array(dkLen);

                        for (let blockNum = 1; blockNum <= numBlocks; blockNum++) {
                            // U1 = HMAC(password, salt || INT(blockNum))
                            const blockBytes = new Uint8Array(4);
                            blockBytes[0] = (blockNum >> 24) & 0xff;
                            blockBytes[1] = (blockNum >> 16) & 0xff;
                            blockBytes[2] = (blockNum >> 8) & 0xff;
                            blockBytes[3] = blockNum & 0xff;

                            const saltAndBlock = new Uint8Array(salt.length + 4);
                            saltAndBlock.set(salt);
                            saltAndBlock.set(blockBytes, salt.length);

                            const keyStr = String.fromCharCode.apply(null, password);
                            let u = globalThis.__edgebox_hmac(hashName, keyStr, String.fromCharCode.apply(null, saltAndBlock));
                            let uBytes = new Uint8Array(u.length / 2);
                            for (let i = 0; i < uBytes.length; i++) {
                                uBytes[i] = parseInt(u.substring(i * 2, i * 2 + 2), 16);
                            }

                            const t = new Uint8Array(uBytes);

                            for (let iter = 1; iter < iterations; iter++) {
                                u = globalThis.__edgebox_hmac(hashName, keyStr, String.fromCharCode.apply(null, uBytes));
                                uBytes = new Uint8Array(u.length / 2);
                                for (let i = 0; i < uBytes.length; i++) {
                                    uBytes[i] = parseInt(u.substring(i * 2, i * 2 + 2), 16);
                                }
                                for (let i = 0; i < t.length; i++) {
                                    t[i] ^= uBytes[i];
                                }
                            }

                            const offset = (blockNum - 1) * hLen;
                            const toCopy = Math.min(hLen, dkLen - offset);
                            result.set(t.subarray(0, toCopy), offset);
                        }

                        return result.buffer;
                    }
                }

                throw new Error('deriveBits not supported for: ' + algoName);
            },

            // deriveKey(algorithm, baseKey, derivedKeyType, extractable, keyUsages) - Derive key
            async deriveKey(algorithm, baseKey, derivedKeyType, extractable, keyUsages) {
                const derivedAlgo = typeof derivedKeyType === 'string' ? derivedKeyType : derivedKeyType.name;
                const length = derivedKeyType.length || 256;

                const bits = await this.deriveBits(algorithm, baseKey, length);
                return this.importKey('raw', bits, derivedKeyType, extractable, keyUsages);
            },

            // encrypt - AES-GCM via native binding
            async encrypt(algorithm, key, data) {
                if (!(key instanceof CryptoKey)) {
                    throw new TypeError('key must be a CryptoKey');
                }
                if (!key.usages.includes('encrypt')) {
                    throw new DOMException('Key does not support encryption', 'InvalidAccessError');
                }

                const algoName = typeof algorithm === 'string' ? algorithm : algorithm.name;

                let dataBytes;
                if (data instanceof ArrayBuffer) {
                    dataBytes = new Uint8Array(data);
                } else if (ArrayBuffer.isView(data)) {
                    dataBytes = new Uint8Array(data.buffer, data.byteOffset, data.byteLength);
                } else {
                    throw new TypeError('data must be BufferSource');
                }

                if (algoName === 'AES-GCM') {
                    if (typeof globalThis.__edgebox_aes_gcm_encrypt !== 'function') {
                        throw new Error('Native AES-GCM not available');
                    }

                    const iv = algorithm.iv;
                    let ivBytes;
                    if (iv instanceof ArrayBuffer) {
                        ivBytes = new Uint8Array(iv);
                    } else if (ArrayBuffer.isView(iv)) {
                        ivBytes = new Uint8Array(iv.buffer, iv.byteOffset, iv.byteLength);
                    } else {
                        throw new TypeError('iv must be BufferSource');
                    }

                    if (ivBytes.length !== 12) {
                        throw new Error('AES-GCM requires 12-byte IV');
                    }

                    const keyBytes = new Uint8Array(key._keyData);
                    if (keyBytes.length !== 32) {
                        throw new Error('Only AES-256-GCM supported (32-byte key)');
                    }

                    const keyStr = String.fromCharCode.apply(null, keyBytes);
                    const ivStr = String.fromCharCode.apply(null, ivBytes);
                    const dataStr = String.fromCharCode.apply(null, dataBytes);

                    const result = globalThis.__edgebox_aes_gcm_encrypt(keyStr, ivStr, dataStr);
                    const bytes = new Uint8Array(result.length);
                    for (let i = 0; i < result.length; i++) bytes[i] = result.charCodeAt(i);
                    return bytes.buffer;
                }

                throw new Error('Encryption algorithm not supported: ' + algoName);
            },

            // decrypt - AES-GCM via native binding
            async decrypt(algorithm, key, data) {
                if (!(key instanceof CryptoKey)) {
                    throw new TypeError('key must be a CryptoKey');
                }
                if (!key.usages.includes('decrypt')) {
                    throw new DOMException('Key does not support decryption', 'InvalidAccessError');
                }

                const algoName = typeof algorithm === 'string' ? algorithm : algorithm.name;

                let dataBytes;
                if (data instanceof ArrayBuffer) {
                    dataBytes = new Uint8Array(data);
                } else if (ArrayBuffer.isView(data)) {
                    dataBytes = new Uint8Array(data.buffer, data.byteOffset, data.byteLength);
                } else {
                    throw new TypeError('data must be BufferSource');
                }

                if (algoName === 'AES-GCM') {
                    if (typeof globalThis.__edgebox_aes_gcm_decrypt !== 'function') {
                        throw new Error('Native AES-GCM not available');
                    }

                    const iv = algorithm.iv;
                    let ivBytes;
                    if (iv instanceof ArrayBuffer) {
                        ivBytes = new Uint8Array(iv);
                    } else if (ArrayBuffer.isView(iv)) {
                        ivBytes = new Uint8Array(iv.buffer, iv.byteOffset, iv.byteLength);
                    } else {
                        throw new TypeError('iv must be BufferSource');
                    }

                    if (ivBytes.length !== 12) {
                        throw new Error('AES-GCM requires 12-byte IV');
                    }

                    const keyBytes = new Uint8Array(key._keyData);
                    if (keyBytes.length !== 32) {
                        throw new Error('Only AES-256-GCM supported (32-byte key)');
                    }

                    const keyStr = String.fromCharCode.apply(null, keyBytes);
                    const ivStr = String.fromCharCode.apply(null, ivBytes);
                    const dataStr = String.fromCharCode.apply(null, dataBytes);

                    const result = globalThis.__edgebox_aes_gcm_decrypt(keyStr, ivStr, dataStr);
                    const bytes = new Uint8Array(result.length);
                    for (let i = 0; i < result.length; i++) bytes[i] = result.charCodeAt(i);
                    return bytes.buffer;
                }

                throw new Error('Decryption algorithm not supported: ' + algoName);
            },

            // wrapKey and unwrapKey using encrypt/decrypt
            async wrapKey(format, key, wrappingKey, wrapAlgorithm) {
                const exported = await this.exportKey(format, key);
                let keyData;
                if (format === 'raw') {
                    keyData = exported;
                } else {
                    keyData = new TextEncoder().encode(JSON.stringify(exported));
                }
                return this.encrypt(wrapAlgorithm, wrappingKey, keyData);
            },

            async unwrapKey(format, wrappedKey, unwrappingKey, unwrapAlgorithm, unwrappedKeyAlgorithm, extractable, keyUsages) {
                const keyData = await this.decrypt(unwrapAlgorithm, unwrappingKey, wrappedKey);
                if (format === 'raw') {
                    return this.importKey('raw', keyData, unwrappedKeyAlgorithm, extractable, keyUsages);
                } else {
                    const jwk = JSON.parse(new TextDecoder().decode(keyData));
                    return this.importKey('jwk', jwk, unwrappedKeyAlgorithm, extractable, keyUsages);
                }
            }
        };

        globalThis.crypto = {
            randomUUID: () => _modules.crypto.randomUUID(),
            getRandomValues: (array) => {
                for (let i = 0; i < array.length; i++) {
                    array[i] = Math.floor(Math.random() * 256);
                }
                return array;
            },
            subtle: subtle
        };

        // Export CryptoKey for instanceof checks
        globalThis.CryptoKey = CryptoKey;
    }

    // Wrap user code execution in try-catch to see any errors
    globalThis.__edgebox_wrapEntry = function(entryFn) {
        return async function() {
            try {
                return await entryFn.apply(this, arguments);
            } catch (e) {
                print('[EDGEBOX ERROR] ' + (e.message || e));
                if (e.stack) print(e.stack);
                throw e;
            }
        };
    };

    // Fix console.log after user bundle overwrites it
    // This runs at end of polyfills, just before user code entry point executes
    globalThis.__fixConsole = function() {
        const _print = typeof print === 'function' ? print : () => {};
        if (typeof globalThis.console !== 'undefined') {
            const origLog = globalThis.console.log;
            // Check if console.log is a no-op (empty function)
            const fnStr = origLog.toString();
            if (fnStr.includes('function()') && fnStr.includes('{}')) {
                print('[CONSOLE FIX] Detected no-op console.log, restoring...');
                globalThis.console.log = (...args) => _print(...args);
            }
        }
    };
    // Schedule console fix for next microtask (after synchronous module init)
    if (typeof queueMicrotask === 'function') {
        queueMicrotask(globalThis.__fixConsole);
    }

    // Mark polyfills as initialized to prevent double-init in Wizer mode
    globalThis._polyfillsInitialized = true;

})();
