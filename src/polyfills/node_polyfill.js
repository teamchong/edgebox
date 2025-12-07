(function() {
    'use strict';

    // Module registry
    const _modules = {};

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
        readdirSync: path => __edgebox_fs_readdir(path),
        statSync: path => __edgebox_fs_stat(path),
        unlinkSync: path => __edgebox_fs_unlink(path),
        rmdirSync: (path, options) => __edgebox_fs_rmdir(path, (options && options.recursive) || false),
        renameSync: (oldPath, newPath) => __edgebox_fs_rename(oldPath, newPath),
        copyFileSync: (src, dest) => __edgebox_fs_copy(src, dest),
        readFile: function(path, options) { return Promise.resolve(this.readFileSync(path, options)); },
        writeFile: function(path, data, options) { return Promise.resolve(this.writeFileSync(path, data, options)); },
        exists: function(path) { return Promise.resolve(this.existsSync(path)); },
        mkdir: function(path, options) { return Promise.resolve(this.mkdirSync(path, options)); },
        readdir: function(path) { return Promise.resolve(this.readdirSync(path)); },
        stat: function(path) { return Promise.resolve(this.statSync(path)); },
        unlink: function(path) { return Promise.resolve(this.unlinkSync(path)); },
        rmdir: function(path, options) { return Promise.resolve(this.rmdirSync(path, options)); },
        rename: function(oldPath, newPath) { return Promise.resolve(this.renameSync(oldPath, newPath)); },
        copyFile: function(src, dest) { return Promise.resolve(this.copyFileSync(src, dest)); },
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
        resolve: (from, to) => new URL(to, from).href
    };

    // ===== PROCESS OBJECT =====
    if (!globalThis.process) globalThis.process = {};
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
        stdout: { write: data => { console.log(data); return true; } },
        stderr: { write: data => { console.error(data); return true; } }
    });

    // ===== REQUIRE FUNCTION =====
    globalThis.require = function(name) {
        if (_modules[name]) return _modules[name];
        throw new Error('Module not found: ' + name);
    };
    globalThis.require.resolve = name => name;

})();
