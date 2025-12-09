(function() {
    'use strict';

    // Module registry - use globalThis._modules for compatibility with require()
    globalThis._modules = globalThis._modules || {};
    const _modules = globalThis._modules;

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
        }
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
            for (const listener of this._events[event].slice()) listener(...args);
            return true;
        }
        listenerCount(event) { return (this._events[event] && this._events[event].length) || 0; }
        listeners(event) { return (this._events[event] || []).map(l => l._original || l); }
        setMaxListeners(n) { this._maxListeners = n; return this; }
        getMaxListeners() { return this._maxListeners; }
    }
    _modules.events = { EventEmitter };

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
    _modules.stream = { Stream, Readable, Writable, Duplex, Transform, PassThrough };

    // ===== FS MODULE =====
    _modules.fs = {
        readFileSync: function(path, options) {
            const encoding = typeof options === 'string' ? options : (options && options.encoding);
            const data = __edgebox_fs_read(path);
            return (encoding === 'utf8' || encoding === 'utf-8') ? data : Buffer.from(data);
        },
        writeFileSync: (path, data) => __edgebox_fs_write(path, String(data)),
        existsSync: path => __edgebox_fs_exists(path),
        mkdirSync: (path, options) => __edgebox_fs_mkdir(path, (options && options.recursive) || false),
        readdirSync: function(path, options) {
            const entries = __edgebox_fs_readdir(path);
            if (options && options.withFileTypes) {
                return entries.map(name => ({
                    name,
                    isFile: () => __edgebox_fs_stat(path + '/' + name).isFile,
                    isDirectory: () => __edgebox_fs_stat(path + '/' + name).isDirectory,
                    isSymbolicLink: () => false
                }));
            }
            return entries;
        },
        statSync: path => __edgebox_fs_stat(path),
        lstatSync: path => __edgebox_fs_stat(path),  // Same as statSync (no symlink support)
        unlinkSync: path => __edgebox_fs_unlink(path),
        rmdirSync: (path, options) => __edgebox_fs_rmdir(path, (options && options.recursive) || false),
        rmSync: function(path, options) { return __edgebox_fs_rmdir(path, (options && options.recursive) || false); },
        renameSync: (oldPath, newPath) => __edgebox_fs_rename(oldPath, newPath),
        copyFileSync: (src, dest) => __edgebox_fs_copy(src, dest),
        // realpathSync - returns the path as-is (no symlink resolution in WASI)
        realpathSync: Object.assign(function(path) { return path; }, { native: function(path) { return path; } }),
        realpath: Object.assign(function(path, opts, cb) {
            if (typeof opts === 'function') { cb = opts; opts = {}; }
            if (cb) cb(null, path);
            return Promise.resolve(path);
        }, { native: function(path, opts, cb) { if (typeof opts === 'function') { cb = opts; } if (cb) cb(null, path); } }),
        // accessSync - check if path exists, throw if not
        accessSync: function(path, mode) {
            if (!__edgebox_fs_exists(path)) {
                const err = new Error('ENOENT: no such file or directory, access \'' + path + '\'');
                err.code = 'ENOENT';
                throw err;
            }
        },
        // Stubs for file descriptors
        openSync: function(path, flags) { return { path, flags, fd: 1 }; },
        closeSync: function(fd) {},
        readSync: function(fd, buffer, offset, length, position) { return 0; },
        writeSync: function(fd, buffer) { return buffer.length; },
        fstatSync: function(fd) { return fd.path ? __edgebox_fs_stat(fd.path) : { isFile: () => true, isDirectory: () => false, size: 0 }; },
        fsyncSync: function(fd) {},
        // Stream factories
        createReadStream: function(path) {
            const content = __edgebox_fs_read(path);
            const stream = new EventEmitter();
            stream.pipe = (dest) => { dest.write(content); dest.end(); return dest; };
            setTimeout(() => { stream.emit('data', content); stream.emit('end'); }, 0);
            return stream;
        },
        createWriteStream: function(path) {
            const chunks = [];
            return {
                write: (chunk) => { chunks.push(chunk); return true; },
                end: (chunk) => { if (chunk) chunks.push(chunk); __edgebox_fs_write(path, chunks.join('')); },
                on: () => {}
            };
        },
        // Constants
        constants: { F_OK: 0, R_OK: 4, W_OK: 2, X_OK: 1, COPYFILE_EXCL: 1 },
        // Async versions
        readFile: function(path, options) { return Promise.resolve(this.readFileSync(path, options)); },
        writeFile: function(path, data, options) { return Promise.resolve(this.writeFileSync(path, data, options)); },
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
        promises: null
    };
    _modules.fs.promises = {
        readFile: _modules.fs.readFile.bind(_modules.fs),
        writeFile: _modules.fs.writeFile.bind(_modules.fs),
        mkdir: _modules.fs.mkdir.bind(_modules.fs),
        readdir: _modules.fs.readdir.bind(_modules.fs),
        stat: _modules.fs.stat.bind(_modules.fs),
        unlink: _modules.fs.unlink.bind(_modules.fs),
        rmdir: _modules.fs.rmdir.bind(_modules.fs),
        rename: _modules.fs.rename.bind(_modules.fs),
        copyFile: _modules.fs.copyFile.bind(_modules.fs),
        access: path => Promise.resolve(_modules.fs.existsSync(path))
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
            return {
                update: function(input) {
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
                            bytes.push(parseInt(result.substr(i, 2), 16));
                        }
                        return btoa(String.fromCharCode.apply(null, bytes));
                    }
                    // Return as Buffer by default
                    const bytes = [];
                    for (let i = 0; i < result.length; i += 2) {
                        bytes.push(parseInt(result.substr(i, 2), 16));
                    }
                    return Buffer.from(bytes);
                }
            };
        },
        // createHmac - returns an Hmac object with update() and digest()
        createHmac: function(algorithm, key) {
            const algo = algorithm.toLowerCase();
            const keyStr = typeof key === 'string' ? key : String.fromCharCode.apply(null, key);
            let data = '';
            return {
                update: function(input) {
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
                            bytes.push(parseInt(result.substr(i, 2), 16));
                        }
                        return btoa(String.fromCharCode.apply(null, bytes));
                    }
                    const bytes = [];
                    for (let i = 0; i < result.length; i += 2) {
                        bytes.push(parseInt(result.substr(i, 2), 16));
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
    _modules.http = {
        IncomingMessage, ServerResponse,
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
        }
    };
    _modules.https = _modules.http;

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
    _modules.os = {
        platform: () => 'wasi',
        arch: () => 'wasm32',
        type: () => 'WASI',
        release: () => '1.0.0',
        hostname: () => 'edgebox',
        homedir: () => typeof __edgebox_homedir === 'function' ? __edgebox_homedir() : '/home/user',
        tmpdir: () => '/tmp',
        cpus: () => [{ model: 'WASM', speed: 0, times: { user: 0, nice: 0, sys: 0, idle: 0, irq: 0 } }],
        totalmem: () => 256 * 1024 * 1024,
        freemem: () => 128 * 1024 * 1024,
        uptime: () => 0,
        loadavg: () => [0, 0, 0],
        networkInterfaces: () => ({}),
        userInfo: () => ({ username: 'user', uid: 1000, gid: 1000, shell: '/bin/sh', homedir: '/home/user' }),
        endianness: () => 'LE',
        EOL: '\n',
        constants: { signals: {}, errno: {} }
    };

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
    const _stdout = {
        isTTY: true,
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
        isTTY: true,
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

    const _stdin = {
        isTTY: true,
        isRaw: false,
        setRawMode: function(mode) { this.isRaw = mode; return this; },
        read: function() { return null; },
        on: function() { return this; },
        once: function() { return this; },
        emit: function() { return false; },
        pause: function() { return this; },
        resume: function() { return this; },
        setEncoding: function() { return this; }
    };

    Object.assign(globalThis.process, {
        env: (globalThis.process && globalThis.process.env) || {},
        argv: (globalThis.process && globalThis.process.argv) || [],
        cwd: () => typeof __edgebox_cwd === 'function' ? __edgebox_cwd() : '/',
        version: 'v18.0.0-edgebox',
        versions: { node: '18.0.0', v8: '0.0.0', quickjs: '2024.1' },
        platform: 'wasi',
        arch: 'wasm32',
        hrtime: { bigint: () => BigInt(Date.now()) * 1000000n },
        nextTick: (fn, ...args) => Promise.resolve().then(() => fn(...args)),
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
    // Net stub
    _modules.net = {
        Socket: class Socket extends EventEmitter {
            constructor() { super(); this.connecting = false; }
            connect() { return this; }
            write() { return true; }
            end() { this.emit('close'); }
            destroy() { this.emit('close'); }
        },
        createServer: () => new EventEmitter(),
        isIP: s => /^\d+\.\d+\.\d+\.\d+$/.test(s) ? 4 : 0
    };

    // Zlib stub
    _modules.zlib = {
        createGzip: () => new PassThrough(),
        createGunzip: () => new PassThrough(),
        createDeflate: () => new PassThrough(),
        createInflate: () => new PassThrough(),
        gzip: (buf, cb) => cb(null, buf),
        gunzip: (buf, cb) => cb(null, buf),
        deflate: (buf, cb) => cb(null, buf),
        inflate: (buf, cb) => cb(null, buf),
        gzipSync: buf => buf,
        gunzipSync: buf => buf,
        deflateSync: buf => buf,
        inflateSync: buf => buf
    };

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

    class AsyncResource {
        constructor(type, options) {
            this.type = type;
            this.asyncId = ++AsyncResource._idCounter;
            this.triggerAsyncId = (options && options.triggerAsyncId) || 0;
        }
        static _idCounter = 0;
        runInAsyncScope(fn, thisArg, ...args) { return fn.apply(thisArg, args); }
        emitDestroy() { return this; }
        asyncId() { return this.asyncId; }
        triggerAsyncId() { return this.triggerAsyncId; }
    }

    _modules.async_hooks = {
        createHook: (callbacks) => ({
            enable: () => {},
            disable: () => {}
        }),
        executionAsyncId: () => 1,
        triggerAsyncId: () => 0,
        AsyncLocalStorage,
        AsyncResource
    };

    // Timers/promises stub
    _modules['timers/promises'] = {
        setTimeout: (delay) => new Promise(resolve => setTimeout(resolve, delay)),
        setImmediate: () => new Promise(resolve => setImmediate(resolve))
    };

    // Child process module
    _modules.child_process = {
        spawnSync: function(cmd, args = [], options = {}) {
            // Use native binding if available (__edgebox_spawn returns { status, stdout, stderr })
            if (typeof __edgebox_spawn === 'function') {
                try {
                    const result = __edgebox_spawn(cmd, JSON.stringify(args || []), JSON.stringify(options.env || {}), options.cwd || null);
                    // Result is a JSON string: { status: number, stdout: string, stderr: string }
                    const parsed = typeof result === 'string' ? JSON.parse(result) : result;
                    return {
                        status: parsed.status || 0,
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

            // Execute synchronously in next tick to simulate async
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

    // DNS stub
    _modules.dns = {
        lookup: (hostname, cb) => cb(null, '127.0.0.1', 4),
        resolve: (hostname, cb) => cb(null, ['127.0.0.1'])
    };

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

    // Worker threads module - stub for WASI (no real threads)
    _modules.worker_threads = {
        isMainThread: true,
        parentPort: null,
        workerData: null,
        threadId: 0,
        Worker: class Worker {
            constructor() { throw new Error('Worker threads not supported in WASI'); }
        },
        MessageChannel: class MessageChannel {
            constructor() {
                this.port1 = new EventEmitter();
                this.port2 = new EventEmitter();
            }
        },
        MessagePort: EventEmitter,
        BroadcastChannel: class BroadcastChannel extends EventEmitter {
            constructor(name) { super(); this.name = name; }
            postMessage() {}
            close() {}
        }
    };

    // Cluster module - stub for WASI (single process)
    _modules.cluster = {
        isMaster: true,
        isPrimary: true,
        isWorker: false,
        workers: {},
        fork: () => { throw new Error('Cluster not supported in WASI'); },
        setupPrimary: () => {},
        setupMaster: () => {},
        disconnect: () => {},
        on: () => {}
    };

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

    // ===== REQUIRE FUNCTION =====
    globalThis.require = function(name) {
        // Strip node: prefix
        let moduleName = name.startsWith('node:') ? name.slice(5) : name;

        // Direct lookup
        if (_modules[moduleName]) return _modules[moduleName];

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
                this._params = new Map();
                if (typeof init === 'string') {
                    init = init.startsWith('?') ? init.slice(1) : init;
                    for (const pair of init.split('&')) {
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
                const match = url.match(/^([a-z]+):\/\/([^/:]+)?(?::(\d+))?(\/[^?#]*)?(\?[^#]*)?(#.*)?$/i);
                if (!match) throw new TypeError('Invalid URL: ' + url);
                this.protocol = match[1] + ':';
                this.hostname = match[2] || '';
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

    // Web Crypto API
    if (typeof globalThis.crypto === 'undefined') {
        globalThis.crypto = {
            randomUUID: () => _modules.crypto.randomUUID(),
            getRandomValues: (array) => {
                for (let i = 0; i < array.length; i++) {
                    array[i] = Math.floor(Math.random() * 256);
                }
                return array;
            },
            subtle: {
                digest: () => Promise.reject(new Error('SubtleCrypto not implemented')),
                encrypt: () => Promise.reject(new Error('SubtleCrypto not implemented')),
                decrypt: () => Promise.reject(new Error('SubtleCrypto not implemented'))
            }
        };
    }

})();
