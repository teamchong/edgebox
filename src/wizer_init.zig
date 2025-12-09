/// Wizer Pre-initialization Support for EdgeBox
///
/// This module provides "instant startup" by pre-initializing the QuickJS
/// runtime at build time. Wizer snapshots the memory state after wizer_init()
/// runs, embedding it into the WASM binary.
///
/// Build pipeline:
/// 1. zig build wasm -> edgebox-base.wasm (with wizer_init export)
/// 2. wizer edgebox-base.wasm --init-func=wizer_init -> edgebox-wizer.wasm
/// 3. wasmedge compile edgebox-wizer.wasm -> edgebox-aot.dylib
///
/// At runtime, main() detects wizer_initialized=true and uses the
/// pre-initialized runtime, skipping JS_NewRuntime/JS_NewContext.
const std = @import("std");
const quickjs = @import("quickjs_core.zig");
const wasm_bump = @import("wasm_bump.zig");

const qjs = quickjs.c;

// ============================================================================
// GLOBAL STATE (Persists across Wizer snapshot)
// ============================================================================

/// Wizer-initialized QuickJS runtime (populated at build time)
pub var wizer_runtime: ?*qjs.JSRuntime = null;

/// Wizer-initialized QuickJS context (populated at build time)
pub var wizer_context: ?*qjs.JSContext = null;

/// Flag indicating Wizer initialization completed successfully
pub var wizer_initialized: bool = false;

// ============================================================================
// WIZER INITIALIZATION (Runs at BUILD TIME)
// ============================================================================

/// Wizer initialization function - exported and called at build time.
/// Creates the QuickJS runtime/context and pre-compiles static polyfills.
///
/// This function MUST NOT:
/// - Open files or sockets (WASI state is not snapshotted)
/// - Call Date.now() or Math.random() (seeds would be fixed)
/// - Read environment variables (dynamic)
/// - Read command line args (dynamic)
pub fn wizer_init() void {
    // Initialize bump allocator for QuickJS memory
    wasm_bump.init();

    // Create malloc functions using our bump allocator
    const malloc_funcs = qjs.JSMallocFunctions{
        .js_calloc = wasm_bump.js_calloc,
        .js_malloc = wasm_bump.js_malloc,
        .js_free = wasm_bump.js_free,
        .js_realloc = wasm_bump.js_realloc,
        .js_malloc_usable_size = wasm_bump.js_malloc_usable_size,
    };

    // Create QuickJS runtime with bump allocator
    wizer_runtime = qjs.JS_NewRuntime2(&malloc_funcs, null);
    if (wizer_runtime == null) return;

    // Create context
    wizer_context = qjs.JS_NewContext(wizer_runtime);
    if (wizer_context == null) return;

    // Initialize std/os modules (module loaders, not fd bindings)
    _ = qjs.js_init_module_std(wizer_context, "std");
    _ = qjs.js_init_module_os(wizer_context, "os");

    // Pre-compile static polyfills (class definitions, pure functions)
    initStaticPolyfills();

    wizer_initialized = true;
}

/// Initialize static polyfills that are safe to snapshot.
/// These are pure class definitions, module stubs, and helpers with no side effects.
/// By moving these to Wizer build-time, we save ~4ms of JS evaluation at runtime.
fn initStaticPolyfills() void {
    const ctx = wizer_context orelse return;

    // Static polyfills - class definitions and module stubs (no WASI/dynamic state)
    const static_polyfills =
        \\// === Static class definitions (safe to snapshot) ===
        \\globalThis.global = globalThis;
        \\globalThis.self = globalThis;
        \\globalThis._modules = {};
        \\globalThis._moduleCache = {};
        \\globalThis._wizerInitialized = true;
        \\
        \\// === require() module loader ===
        \\globalThis.require = function(id) {
        \\    var name = id.replace(/^node:/, '');
        \\    if (globalThis._moduleCache[id]) return globalThis._moduleCache[id];
        \\    if (globalThis._moduleCache[name]) return globalThis._moduleCache[name];
        \\    if (globalThis._modules[id] !== undefined) {
        \\        globalThis._moduleCache[id] = globalThis._modules[id];
        \\        return globalThis._modules[id];
        \\    }
        \\    if (globalThis._modules[name] !== undefined) {
        \\        globalThis._moduleCache[name] = globalThis._modules[name];
        \\        return globalThis._modules[name];
        \\    }
        \\    throw new Error('Module not found: ' + id);
        \\};
        \\
        \\// === TextEncoder (pure string->bytes, safe to snapshot) ===
        \\if (typeof TextEncoder === 'undefined') {
        \\    globalThis.TextEncoder = class TextEncoder {
        \\        constructor(encoding = 'utf-8') { this.encoding = encoding; }
        \\        encode(str) {
        \\            var bytes = [];
        \\            for (var i = 0; i < str.length; i++) {
        \\                var code = str.charCodeAt(i);
        \\                if (code < 0x80) bytes.push(code);
        \\                else if (code < 0x800) {
        \\                    bytes.push(0xC0 | (code >> 6));
        \\                    bytes.push(0x80 | (code & 0x3F));
        \\                } else {
        \\                    bytes.push(0xE0 | (code >> 12));
        \\                    bytes.push(0x80 | ((code >> 6) & 0x3F));
        \\                    bytes.push(0x80 | (code & 0x3F));
        \\                }
        \\            }
        \\            return new Uint8Array(bytes);
        \\        }
        \\    };
        \\}
        \\
        \\// === TextDecoder (pure bytes->string, safe to snapshot) ===
        \\if (typeof TextDecoder === 'undefined') {
        \\    globalThis.TextDecoder = class TextDecoder {
        \\        constructor(encoding = 'utf-8') { this.encoding = encoding; }
        \\        decode(input) {
        \\            var bytes = input instanceof Uint8Array ? input : new Uint8Array(input);
        \\            var str = '';
        \\            for (var i = 0; i < bytes.length; ) {
        \\                var b = bytes[i++];
        \\                if (b < 0x80) str += String.fromCharCode(b);
        \\                else if (b < 0xE0) str += String.fromCharCode(((b & 0x1F) << 6) | (bytes[i++] & 0x3F));
        \\                else str += String.fromCharCode(((b & 0x0F) << 12) | ((bytes[i++] & 0x3F) << 6) | (bytes[i++] & 0x3F));
        \\            }
        \\            return str;
        \\        }
        \\    };
        \\}
        \\
        \\// === URL/URLSearchParams (pure string parsing, safe to snapshot) ===
        \\if (typeof URLSearchParams === 'undefined') {
        \\    globalThis.URLSearchParams = class URLSearchParams {
        \\        constructor(init) {
        \\            this._params = [];
        \\            if (typeof init === 'string') {
        \\                (init.startsWith('?') ? init.slice(1) : init).split('&').forEach(function(p) {
        \\                    var parts = p.split('=').map(decodeURIComponent);
        \\                    if (parts[0]) this._params.push([parts[0], parts[1] || '']);
        \\                }.bind(this));
        \\            }
        \\        }
        \\        get(name) { var p = this._params.find(function(x) { return x[0] === name; }); return p ? p[1] : null; }
        \\        has(name) { return this._params.some(function(x) { return x[0] === name; }); }
        \\        toString() { return this._params.map(function(x) { return encodeURIComponent(x[0]) + '=' + encodeURIComponent(x[1]); }).join('&'); }
        \\    };
        \\}
        \\
        \\if (typeof URL === 'undefined') {
        \\    globalThis.URL = class URL {
        \\        constructor(url, base) {
        \\            var m = url.match(/^(([^:/?#]+):)?(\/\/([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?/);
        \\            this.protocol = (m[2] || '') + ':';
        \\            this.host = m[4] || '';
        \\            this.hostname = this.host.split(':')[0];
        \\            this.port = this.host.split(':')[1] || '';
        \\            this.pathname = m[5] || '/';
        \\            this.search = m[6] || '';
        \\            this.hash = m[8] || '';
        \\            this.searchParams = new URLSearchParams(this.search);
        \\        }
        \\        get origin() { return this.protocol + '//' + this.host; }
        \\        get href() { return this.origin + this.pathname + this.search + this.hash; }
        \\        toString() { return this.href; }
        \\    };
        \\}
        \\
        \\// === Event classes (pure, safe to snapshot) ===
        \\if (typeof Event === 'undefined') {
        \\    globalThis.Event = class Event {
        \\        constructor(type, opts) {
        \\            opts = opts || {};
        \\            this.type = type;
        \\            this.bubbles = opts.bubbles || false;
        \\            this.defaultPrevented = false;
        \\        }
        \\        preventDefault() { this.defaultPrevented = true; }
        \\    };
        \\    globalThis.CustomEvent = class CustomEvent extends Event {
        \\        constructor(type, opts) { super(type, opts); this.detail = opts ? opts.detail : undefined; }
        \\    };
        \\    globalThis.EventTarget = class EventTarget {
        \\        constructor() { this._l = {}; }
        \\        addEventListener(t, fn) { (this._l[t] = this._l[t] || []).push(fn); }
        \\        removeEventListener(t, fn) { if (this._l[t]) this._l[t] = this._l[t].filter(function(f) { return f !== fn; }); }
        \\        dispatchEvent(e) { (this._l[e.type] || []).forEach(function(fn) { fn(e); }); return !e.defaultPrevented; }
        \\    };
        \\}
        \\
        \\// === DOMException (pure error class) ===
        \\if (typeof DOMException === 'undefined') {
        \\    globalThis.DOMException = class DOMException extends Error {
        \\        constructor(msg, name) { super(msg); this.name = name || 'Error'; }
        \\    };
        \\}
        \\
        \\// === path module (pure string operations) ===
        \\globalThis._modules['path'] = {
        \\    sep: '/',
        \\    delimiter: ':',
        \\    basename: function(p, ext) { var b = p.split('/').pop() || ''; return ext && b.endsWith(ext) ? b.slice(0, -ext.length) : b; },
        \\    dirname: function(p) { var parts = p.split('/'); parts.pop(); return parts.join('/') || '/'; },
        \\    extname: function(p) { var b = this.basename(p); var i = b.lastIndexOf('.'); return i > 0 ? b.slice(i) : ''; },
        \\    join: function() { return Array.prototype.slice.call(arguments).join('/').replace(/\/+/g, '/'); },
        \\    resolve: function() { var r = ''; for (var i = 0; i < arguments.length; i++) { var p = arguments[i]; if (p.startsWith('/')) r = p; else r = r + '/' + p; } return this.normalize(r || '/'); },
        \\    normalize: function(p) { var parts = p.split('/').filter(Boolean); var result = []; for (var i = 0; i < parts.length; i++) { if (parts[i] === '..') result.pop(); else if (parts[i] !== '.') result.push(parts[i]); } return (p.startsWith('/') ? '/' : '') + result.join('/'); },
        \\    isAbsolute: function(p) { return p.startsWith('/'); },
        \\    relative: function(from, to) { return to; },
        \\    parse: function(p) { return { root: p.startsWith('/') ? '/' : '', dir: this.dirname(p), base: this.basename(p), ext: this.extname(p), name: this.basename(p, this.extname(p)) }; },
        \\    format: function(obj) { return (obj.dir || obj.root || '') + '/' + (obj.base || obj.name + (obj.ext || '')); },
        \\    posix: null,
        \\    win32: null
        \\};
        \\globalThis._modules['path'].posix = globalThis._modules['path'];
        \\globalThis._modules['path'].win32 = globalThis._modules['path'];
        \\globalThis._modules['node:path'] = globalThis._modules['path'];
        \\
        \\// === os module (pure functions, homedir/cwd bound at runtime) ===
        \\globalThis._modules['os'] = {
        \\    platform: function() { return 'linux'; },
        \\    type: function() { return 'Linux'; },
        \\    arch: function() { return 'x64'; },
        \\    release: function() { return '5.0.0'; },
        \\    version: function() { return 'Linux 5.0.0'; },
        \\    hostname: function() { return 'edgebox'; },
        \\    homedir: function() { return globalThis.process ? globalThis.process.env.HOME || '/' : '/'; },
        \\    tmpdir: function() { return '/tmp'; },
        \\    userInfo: function() { return { username: 'root', uid: 0, gid: 0, shell: '/bin/sh', homedir: this.homedir() }; },
        \\    cpus: function() { return [{ model: 'WASM', speed: 1000, times: { user: 0, nice: 0, sys: 0, idle: 0, irq: 0 } }]; },
        \\    totalmem: function() { return 1073741824; },
        \\    freemem: function() { return 536870912; },
        \\    networkInterfaces: function() { return {}; },
        \\    loadavg: function() { return [0, 0, 0]; },
        \\    uptime: function() { return 0; },
        \\    endianness: function() { return 'LE'; },
        \\    EOL: '\n',
        \\    constants: { signals: {}, errno: {}, priority: {} }
        \\};
        \\globalThis._modules['node:os'] = globalThis._modules['os'];
        \\
        \\// === process stub (bound dynamically at runtime for env/argv) ===
        \\globalThis.process = {
        \\    platform: 'wasi',
        \\    arch: 'wasm32',
        \\    version: 'v20.0.0',
        \\    versions: { node: '20.0.0', v8: '11.0.0' },
        \\    pid: 1,
        \\    ppid: 0,
        \\    argv: ['node'],
        \\    argv0: 'node',
        \\    execPath: '/usr/bin/edgebox',
        \\    execArgv: [],
        \\    env: {},
        \\    cwd: function() { return this.env.PWD || '/'; },
        \\    chdir: function(dir) { this.env.PWD = dir; },
        \\    exit: function(code) { if (typeof std !== 'undefined') std.exit(code || 0); },
        \\    on: function() { return this; },
        \\    once: function() { return this; },
        \\    off: function() { return this; },
        \\    emit: function() { return false; },
        \\    removeListener: function() { return this; },
        \\    removeAllListeners: function() { return this; },
        \\    listeners: function() { return []; },
        \\    listenerCount: function() { return 0; },
        \\    nextTick: function(fn) { setTimeout(fn, 0); },
        \\    hrtime: function(prev) { var now = Date.now(); var sec = Math.floor(now / 1000); var nsec = (now % 1000) * 1e6; if (prev) return [sec - prev[0], nsec - prev[1]]; return [sec, nsec]; },
        \\    uptime: function() { return 0; },
        \\    memoryUsage: function() { return { rss: 0, heapTotal: 0, heapUsed: 0, external: 0 }; },
        \\    cpuUsage: function() { return { user: 0, system: 0 }; },
        \\    stdin: { isTTY: false, setRawMode: function() {}, on: function() {}, once: function() {}, read: function() { return null; } },
        \\    stdout: { isTTY: true, write: function(s) { if (typeof print !== 'undefined') print(s); }, columns: 80, rows: 24, on: function() {} },
        \\    stderr: { isTTY: true, write: function(s) { if (typeof print !== 'undefined') print(s); }, columns: 80, rows: 24, on: function() {} }
        \\};
        \\globalThis.process.hrtime.bigint = function() { return BigInt(Date.now()) * 1000000n; };
        \\globalThis._modules['process'] = globalThis.process;
        \\globalThis._modules['node:process'] = globalThis.process;
        \\
        \\// === crypto module (uses native __edgebox_hash when available) ===
        \\globalThis._modules['crypto'] = {
        \\    randomBytes: function(size) {
        \\        var buf = new Uint8Array(size);
        \\        for (var i = 0; i < size; i++) buf[i] = Math.floor(Math.random() * 256);
        \\        return globalThis.Buffer ? globalThis.Buffer.from(buf) : buf;
        \\    },
        \\    randomUUID: function() {
        \\        return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
        \\            var r = Math.random() * 16 | 0;
        \\            return (c === 'x' ? r : (r & 0x3 | 0x8)).toString(16);
        \\        });
        \\    },
        \\    createHash: function(algorithm) {
        \\        var algo = algorithm.toLowerCase();
        \\        return {
        \\            _algorithm: algo,
        \\            _data: '',
        \\            update: function(data) { this._data += (typeof data === 'string' ? data : String(data)); return this; },
        \\            digest: function(encoding) {
        \\                var hex = typeof __edgebox_hash !== 'undefined' ? __edgebox_hash(this._algorithm, this._data) : '0'.repeat(64);
        \\                if (encoding === 'hex') return hex;
        \\                if (encoding === 'base64') {
        \\                    var bytes = [];
        \\                    for (var i = 0; i < hex.length; i += 2) bytes.push(parseInt(hex.substr(i, 2), 16));
        \\                    return btoa(String.fromCharCode.apply(null, bytes));
        \\                }
        \\                return hex;
        \\            }
        \\        };
        \\    },
        \\    createHmac: function(algorithm, key) {
        \\        var algo = algorithm.toLowerCase();
        \\        var keyStr = typeof key === 'string' ? key : String(key);
        \\        return {
        \\            _algorithm: algo,
        \\            _key: keyStr,
        \\            _data: '',
        \\            update: function(data) { this._data += (typeof data === 'string' ? data : String(data)); return this; },
        \\            digest: function(encoding) {
        \\                var hex = typeof __edgebox_hmac !== 'undefined' ? __edgebox_hmac(this._algorithm, this._key, this._data) : '0'.repeat(64);
        \\                if (encoding === 'hex') return hex;
        \\                if (encoding === 'base64') {
        \\                    var bytes = [];
        \\                    for (var i = 0; i < hex.length; i += 2) bytes.push(parseInt(hex.substr(i, 2), 16));
        \\                    return btoa(String.fromCharCode.apply(null, bytes));
        \\                }
        \\                return hex;
        \\            }
        \\        };
        \\    },
        \\    getHashes: function() { return ['sha256', 'sha384', 'sha512', 'sha1', 'md5']; }
        \\};
        \\globalThis._modules['node:crypto'] = globalThis._modules['crypto'];
        \\
        \\// === http/https stubs ===
        \\var HttpAgent = function(opts) { this.options = opts || {}; };
        \\HttpAgent.prototype.createConnection = function() { throw new Error('http Agent not implemented'); };
        \\HttpAgent.prototype.destroy = function() {};
        \\
        \\globalThis._modules['http'] = {
        \\    Agent: HttpAgent,
        \\    request: function() { throw new Error('http.request not implemented - use fetch()'); },
        \\    get: function() { throw new Error('http.get not implemented - use fetch()'); },
        \\    createServer: function() { throw new Error('http.createServer not implemented'); },
        \\    globalAgent: new HttpAgent(),
        \\    METHODS: ['GET','POST','PUT','DELETE','PATCH','HEAD','OPTIONS'],
        \\    STATUS_CODES: { 200: 'OK', 201: 'Created', 204: 'No Content', 301: 'Moved Permanently', 302: 'Found', 304: 'Not Modified', 400: 'Bad Request', 401: 'Unauthorized', 403: 'Forbidden', 404: 'Not Found', 500: 'Internal Server Error' }
        \\};
        \\globalThis._modules['node:http'] = globalThis._modules['http'];
        \\globalThis._modules['https'] = Object.assign({}, globalThis._modules['http'], { Agent: HttpAgent, globalAgent: new HttpAgent() });
        \\globalThis._modules['node:https'] = globalThis._modules['https'];
        \\
        \\// === assert module ===
        \\globalThis._modules['assert'] = function(val, msg) { if (!val) throw new Error(msg || 'Assertion failed'); };
        \\globalThis._modules['assert'].ok = globalThis._modules['assert'];
        \\globalThis._modules['assert'].strictEqual = function(a, b, msg) { if (a !== b) throw new Error(msg || 'Not strictly equal'); };
        \\globalThis._modules['assert'].deepStrictEqual = function(a, b, msg) { if (JSON.stringify(a) !== JSON.stringify(b)) throw new Error(msg || 'Not deeply equal'); };
        \\globalThis._modules['node:assert'] = globalThis._modules['assert'];
        \\
        \\// === querystring module ===
        \\globalThis._modules['querystring'] = {
        \\    parse: function(str) { var obj = {}; str.split('&').forEach(function(p) { var kv = p.split('='); if (kv[0]) obj[decodeURIComponent(kv[0])] = decodeURIComponent(kv[1] || ''); }); return obj; },
        \\    stringify: function(obj) { return Object.keys(obj).map(function(k) { return encodeURIComponent(k) + '=' + encodeURIComponent(obj[k]); }).join('&'); }
        \\};
        \\globalThis._modules['node:querystring'] = globalThis._modules['querystring'];
        \\
        \\// === zlib stubs ===
        \\globalThis._modules['zlib'] = {
        \\    gzipSync: function(data) { return data; },
        \\    gunzipSync: function(data) { return data; },
        \\    deflateSync: function(data) { return data; },
        \\    inflateSync: function(data) { return data; }
        \\};
        \\globalThis._modules['node:zlib'] = globalThis._modules['zlib'];
        \\
        \\// === tty module ===
        \\globalThis._modules['tty'] = {
        \\    isatty: function(fd) { return fd === 0 || fd === 1 || fd === 2; },
        \\    ReadStream: function() {},
        \\    WriteStream: function() {}
        \\};
        \\globalThis._modules['node:tty'] = globalThis._modules['tty'];
        \\
        \\// === readline module ===
        \\globalThis._modules['readline'] = {
        \\    createInterface: function(options) {
        \\        return {
        \\            on: function(event, cb) { return this; },
        \\            question: function(prompt, cb) { cb(''); },
        \\            close: function() {},
        \\            prompt: function() {}
        \\        };
        \\    }
        \\};
        \\globalThis._modules['node:readline'] = globalThis._modules['readline'];
        \\
        \\// === stream module stub ===
        \\globalThis._modules['stream'] = {
        \\    Readable: function() {},
        \\    Writable: function() {},
        \\    Transform: function() {},
        \\    PassThrough: function() {},
        \\    pipeline: function() { throw new Error('stream.pipeline not implemented'); },
        \\    finished: function() { throw new Error('stream.finished not implemented'); }
        \\};
        \\globalThis._modules['node:stream'] = globalThis._modules['stream'];
        \\
        \\// === events module ===
        \\globalThis._modules['events'] = {
        \\    EventEmitter: class EventEmitter {
        \\        constructor() { this._events = {}; }
        \\        on(event, listener) { (this._events[event] = this._events[event] || []).push(listener); return this; }
        \\        once(event, listener) { var self = this; var fn = function() { self.removeListener(event, fn); listener.apply(this, arguments); }; this.on(event, fn); return this; }
        \\        emit(event) { var args = Array.prototype.slice.call(arguments, 1); (this._events[event] || []).forEach(function(fn) { fn.apply(null, args); }); return this._events[event] && this._events[event].length > 0; }
        \\        removeListener(event, listener) { if (this._events[event]) this._events[event] = this._events[event].filter(function(fn) { return fn !== listener; }); return this; }
        \\        removeAllListeners(event) { if (event) delete this._events[event]; else this._events = {}; return this; }
        \\        listeners(event) { return this._events[event] || []; }
        \\        listenerCount(event) { return (this._events[event] || []).length; }
        \\    }
        \\};
        \\globalThis._modules['node:events'] = globalThis._modules['events'];
        \\
        \\// === util module ===
        \\globalThis._modules['util'] = {
        \\    promisify: function(fn) { return function() { var args = Array.prototype.slice.call(arguments); return new Promise(function(resolve, reject) { args.push(function(err, result) { if (err) reject(err); else resolve(result); }); fn.apply(null, args); }); }; },
        \\    format: function(fmt) { var args = Array.prototype.slice.call(arguments, 1); var i = 0; return String(fmt).replace(/%[sdj%]/g, function(m) { if (m === '%%') return '%'; if (i >= args.length) return m; switch (m) { case '%s': return String(args[i++]); case '%d': return Number(args[i++]); case '%j': return JSON.stringify(args[i++]); default: return m; } }); },
        \\    inspect: function(obj) { return JSON.stringify(obj, null, 2); },
        \\    inherits: function(ctor, superCtor) { ctor.prototype = Object.create(superCtor.prototype); ctor.prototype.constructor = ctor; },
        \\    debuglog: function() { return function() {}; },
        \\    deprecate: function(fn) { return fn; },
        \\    types: { isDate: function(v) { return v instanceof Date; }, isRegExp: function(v) { return v instanceof RegExp; }, isArray: Array.isArray }
        \\};
        \\globalThis._modules['node:util'] = globalThis._modules['util'];
        \\
        \\// === Buffer class (simplified) ===
        \\if (typeof globalThis.Buffer === 'undefined') {
        \\    globalThis.Buffer = class Buffer extends Uint8Array {
        \\        static from(data, encoding) {
        \\            if (typeof data === 'string') {
        \\                if (encoding === 'base64') {
        \\                    var str = atob(data);
        \\                    var buf = new Buffer(str.length);
        \\                    for (var i = 0; i < str.length; i++) buf[i] = str.charCodeAt(i);
        \\                    return buf;
        \\                }
        \\                var encoder = new TextEncoder();
        \\                return new Buffer(encoder.encode(data));
        \\            }
        \\            return new Buffer(data);
        \\        }
        \\        static alloc(size, fill) { var buf = new Buffer(size); if (fill !== undefined) buf.fill(fill); return buf; }
        \\        static allocUnsafe(size) { return new Buffer(size); }
        \\        static concat(list, totalLength) {
        \\            if (totalLength === undefined) totalLength = list.reduce(function(a, b) { return a + b.length; }, 0);
        \\            var result = new Buffer(totalLength);
        \\            var offset = 0;
        \\            for (var i = 0; i < list.length; i++) { result.set(list[i], offset); offset += list[i].length; }
        \\            return result;
        \\        }
        \\        static isBuffer(obj) { return obj instanceof Buffer || obj instanceof Uint8Array; }
        \\        toString(encoding) {
        \\            if (encoding === 'base64') {
        \\                var str = '';
        \\                for (var i = 0; i < this.length; i++) str += String.fromCharCode(this[i]);
        \\                return btoa(str);
        \\            }
        \\            return new TextDecoder().decode(this);
        \\        }
        \\        write(str, offset, length, encoding) {
        \\            var encoder = new TextEncoder();
        \\            var bytes = encoder.encode(str);
        \\            this.set(bytes.slice(0, length || bytes.length), offset || 0);
        \\            return Math.min(bytes.length, length || bytes.length);
        \\        }
        \\        copy(target, targetStart, sourceStart, sourceEnd) {
        \\            target.set(this.slice(sourceStart || 0, sourceEnd), targetStart || 0);
        \\        }
        \\        slice(start, end) { return new Buffer(Uint8Array.prototype.slice.call(this, start, end)); }
        \\    };
        \\    globalThis._modules['buffer'] = { Buffer: globalThis.Buffer };
        \\    globalThis._modules['node:buffer'] = globalThis._modules['buffer'];
        \\}
        \\
        \\// === console polyfill ===
        \\if (typeof console === 'undefined') {
        \\    globalThis.console = {
        \\        log: function() { if (typeof print !== 'undefined') print.apply(null, arguments); },
        \\        error: function() { if (typeof print !== 'undefined') print.apply(null, arguments); },
        \\        warn: function() { if (typeof print !== 'undefined') print.apply(null, arguments); },
        \\        info: function() { if (typeof print !== 'undefined') print.apply(null, arguments); },
        \\        debug: function() { if (typeof print !== 'undefined') print.apply(null, arguments); },
        \\        trace: function() { if (typeof print !== 'undefined') print.apply(null, arguments); },
        \\        dir: function(obj) { if (typeof print !== 'undefined') print(JSON.stringify(obj, null, 2)); },
        \\        time: function() {},
        \\        timeEnd: function() {},
        \\        assert: function(cond, msg) { if (!cond && typeof print !== 'undefined') print('Assertion failed:', msg); }
        \\    };
        \\}
        \\
        \\// === fs module stub (WASI bindings added at runtime via _std/_os) ===
        \\globalThis._modules['fs'] = {
        \\    _std: null, _os: null,
        \\    existsSync: function(p) { try { return this._os.stat(p)[1] === 0; } catch(e) { return false; } },
        \\    readFileSync: function(p, o) { var e = typeof o === 'string' ? o : (o && o.encoding); var c = this._std.loadFile(p); if (c === null) { var err = new Error('ENOENT: ' + p); err.code = 'ENOENT'; throw err; } return (e === 'utf8' || e === 'utf-8') ? c : (globalThis.Buffer ? globalThis.Buffer.from(c) : c); },
        \\    writeFileSync: function(p, d) { var f = this._std.open(p, 'w'); if (!f) throw new Error('ENOENT: ' + p); f.puts(typeof d === 'string' ? d : d.toString()); f.close(); },
        \\    appendFileSync: function(p, d) { var f = this._std.open(p, 'a'); if (!f) throw new Error('ENOENT: ' + p); f.puts(typeof d === 'string' ? d : d.toString()); f.close(); },
        \\    unlinkSync: function(p) { try { this._os.remove(p); } catch(e) { var err = new Error('ENOENT: ' + p); err.code = 'ENOENT'; throw err; } },
        \\    mkdirSync: function(p, o) { try { this._os.mkdir(p); } catch(e) { if (!o || !o.recursive) throw e; } },
        \\    rmdirSync: function(p) { try { this._os.remove(p); } catch(e) { throw new Error('ENOENT: ' + p); } },
        \\    readdirSync: function(p, o) { try { var r = this._os.readdir(p); if (r[1] !== 0) throw new Error('ENOENT: ' + p); var e = r[0].filter(function(x) { return x !== '.' && x !== '..'; }); if (o && o.withFileTypes) { var s = this; return e.map(function(x) { var st = s.statSync(p + '/' + x); return { name: x, isFile: function() { return st.isFile(); }, isDirectory: function() { return st.isDirectory(); }, isSymbolicLink: function() { return st.isSymbolicLink(); } }; }); } return e; } catch(e) { var err = new Error('ENOENT: ' + p); err.code = 'ENOENT'; throw err; } },
        \\    statSync: function(p) { try { var r = this._os.stat(p); if (r[1] !== 0) throw new Error('ENOENT: ' + p); var s = r[0]; return { isFile: function() { return (s.mode & 0o170000) === 0o100000; }, isDirectory: function() { return (s.mode & 0o170000) === 0o040000; }, isSymbolicLink: function() { return (s.mode & 0o170000) === 0o120000; }, size: s.size, mtime: new Date(s.mtime * 1000), atime: new Date(s.atime * 1000), ctime: new Date(s.ctime * 1000), mode: s.mode }; } catch(e) { var err = new Error('ENOENT: ' + p); err.code = 'ENOENT'; throw err; } },
        \\    lstatSync: function(p) { return this.statSync(p); },
        \\    realpathSync: Object.assign(function(p) { return p; }, { native: function(p) { return p; } }),
        \\    realpath: Object.assign(function(p, o, c) { if (typeof o === 'function') c = o; if (c) c(null, p); }, { native: function(p, o, c) { if (typeof o === 'function') c = o; if (c) c(null, p); } }),
        \\    copyFileSync: function(s, d) { this.writeFileSync(d, this.readFileSync(s)); },
        \\    renameSync: function(o, n) { try { this._os.rename(o, n); } catch(e) { throw new Error('ENOENT: ' + o); } },
        \\    chmodSync: function() {},
        \\    accessSync: function(p) { if (!this.existsSync(p)) { var err = new Error('ENOENT: ' + p); err.code = 'ENOENT'; throw err; } },
        \\    openSync: function(p, f) { return { path: p, flags: f }; },
        \\    closeSync: function() {},
        \\    readSync: function() { return 0; },
        \\    writeSync: function() { return 0; },
        \\    fstatSync: function(f) { return this.statSync(f.path || '.'); },
        \\    fsyncSync: function() {},
        \\    linkSync: function(e, n) { this._os.symlink(e, n); },
        \\    symlinkSync: function(t, p) { this._os.symlink(t, p); },
        \\    readlinkSync: function(p) { try { return this._os.readlink(p)[0]; } catch(e) { var err = new Error('ENOENT: ' + p); err.code = 'ENOENT'; throw err; } },
        \\    rmSync: function(p, o) { try { this._os.remove(p); } catch(e) { if (!o || !o.force) throw e; } },
        \\    createWriteStream: function(p) { var f = this._std.open(p, 'w'); return { write: function(d) { if (f) f.puts(typeof d === 'string' ? d : d.toString()); }, end: function() { if (f) f.close(); }, on: function() { return this; } }; },
        \\    createReadStream: function(p) { var c = this._std.loadFile(p), e = false; return { on: function(ev, cb) { if (ev === 'data' && !e) { e = true; cb(c); } if (ev === 'end') setTimeout(cb, 0); return this; }, pipe: function(d) { d.write(c); d.end(); return d; } }; },
        \\    constants: { F_OK: 0, R_OK: 4, W_OK: 2, X_OK: 1 },
        \\    promises: null
        \\};
        \\// fs.promises (references back to sync methods)
        \\globalThis._modules['fs'].promises = {
        \\    readFile: function(p, o) { return Promise.resolve(globalThis._modules.fs.readFileSync(p, o)); },
        \\    writeFile: function(p, d, o) { return Promise.resolve(globalThis._modules.fs.writeFileSync(p, d, o)); },
        \\    unlink: function(p) { return Promise.resolve(globalThis._modules.fs.unlinkSync(p)); },
        \\    mkdir: function(p, o) { return Promise.resolve(globalThis._modules.fs.mkdirSync(p, o)); },
        \\    rmdir: function(p) { return Promise.resolve(globalThis._modules.fs.rmdirSync(p)); },
        \\    readdir: function(p, o) { return Promise.resolve(globalThis._modules.fs.readdirSync(p, o)); },
        \\    stat: function(p) { return Promise.resolve(globalThis._modules.fs.statSync(p)); },
        \\    lstat: function(p) { return Promise.resolve(globalThis._modules.fs.lstatSync(p)); },
        \\    realpath: function(p) { return Promise.resolve(globalThis._modules.fs.realpathSync(p)); },
        \\    copyFile: function(s, d) { return Promise.resolve(globalThis._modules.fs.copyFileSync(s, d)); },
        \\    rename: function(o, n) { return Promise.resolve(globalThis._modules.fs.renameSync(o, n)); },
        \\    access: function(p) { return Promise.resolve(globalThis._modules.fs.accessSync(p)); },
        \\    rm: function(p, o) { return Promise.resolve(globalThis._modules.fs.rmSync(p, o)); }
        \\};
    ;

    const val = qjs.JS_Eval(
        ctx,
        static_polyfills.ptr,
        static_polyfills.len,
        "<wizer-static>",
        qjs.JS_EVAL_TYPE_GLOBAL,
    );
    qjs.JS_FreeValue(ctx, val);
}

// ============================================================================
// RUNTIME HELPERS (Called at runtime to bind dynamic state)
// ============================================================================

/// Check if we're running with Wizer pre-initialization
pub fn isWizerInitialized() bool {
    return wizer_initialized and wizer_runtime != null and wizer_context != null;
}

/// Get the pre-initialized context (for use in main())
pub fn getContext() ?*qjs.JSContext {
    return wizer_context;
}

/// Get the pre-initialized runtime
pub fn getRuntime() ?*qjs.JSRuntime {
    return wizer_runtime;
}
