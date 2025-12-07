/// EdgeBox WASM Entry Point
/// QuickJS runtime for WASI with networking support
const std = @import("std");
const quickjs = @import("quickjs_core.zig");
const wasm_fetch = @import("wasm_fetch.zig");
const wasi_tty = @import("wasi_tty.zig");
const wasi_process = @import("wasi_process.zig");

// Global allocator for native bindings
var global_allocator: ?std.mem.Allocator = null;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    global_allocator = allocator;

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print(
            \\EdgeBox - QuickJS WASM Runtime (WasmEdge)
            \\
            \\Usage:
            \\  edgebox <script.js>     Run JavaScript file
            \\  edgebox -e "<code>"     Evaluate JavaScript code
            \\
            \\Features:
            \\  - QuickJS JavaScript engine
            \\  - WASI filesystem access
            \\  - Network sockets (WasmEdge)
            \\
        , .{});
        return;
    }

    const cmd = args[1];
    const is_eval = std.mem.eql(u8, cmd, "-e") and args.len > 2;

    // Determine script args to pass to JS
    // For eval mode: args after -e "<code>"
    // For file mode: args after <script.js>
    const script_args_start: usize = if (is_eval) 3 else 2;
    const script_args = if (script_args_start < args.len) args[script_args_start..] else &[_][:0]const u8{};

    // Convert Zig args to C-style argv for js_std_add_helpers
    // scriptArgs in QuickJS = argv[1:] when using js_std_add_helpers
    // So we prepend the script name to make scriptArgs work correctly
    var c_argv = try allocator.alloc([*c]u8, script_args.len + 1);
    defer allocator.free(c_argv);

    // First element is the script name (or "-e" for eval mode)
    const script_name = if (is_eval) "-e" else cmd;
    c_argv[0] = @constCast(@ptrCast(script_name.ptr));

    // Rest are the script arguments
    for (script_args, 0..) |arg, i| {
        c_argv[i + 1] = @constCast(@ptrCast(arg.ptr));
    }

    // Create QuickJS runtime with std module (print, console, etc.)
    var runtime = try quickjs.Runtime.init(allocator);
    defer runtime.deinit();

    var context = try runtime.newStdContextWithArgs(@intCast(c_argv.len), c_argv.ptr);
    defer context.deinit();

    // Inject polyfills and native bindings
    injectPolyfills(&context) catch |err| {
        std.debug.print("Warning: Failed to inject polyfills: {}\n", .{err});
    };

    if (is_eval) {
        // Eval mode
        const result = context.eval(args[2]) catch |err| {
            std.debug.print("Error: {}\n", .{err});
            std.process.exit(1);
        };
        defer result.free();

        if (!result.isUndefined()) {
            if (result.toStringSlice()) |str| {
                std.debug.print("{s}\n", .{str});
            }
        }
    } else {
        // Run file
        const code = std.fs.cwd().readFileAlloc(allocator, cmd, 50 * 1024 * 1024) catch |err| {
            std.debug.print("Error reading {s}: {}\n", .{ cmd, err });
            std.process.exit(1);
        };
        defer allocator.free(code);

        const result = context.eval(code) catch |err| {
            std.debug.print("Error: {}\n", .{err});
            std.process.exit(1);
        };
        defer result.free();
    }
}

/// Inject JavaScript polyfills for Node.js compatibility
fn injectPolyfills(context: *quickjs.Context) !void {
    // Register native fetch function
    context.registerGlobalFunction("__edgebox_fetch", nativeFetch, 4);

    // Register TTY functions
    context.registerGlobalFunction("__edgebox_isatty", nativeIsatty, 1);
    context.registerGlobalFunction("__edgebox_get_terminal_size", nativeGetTerminalSize, 0);
    context.registerGlobalFunction("__edgebox_read_stdin", nativeReadStdin, 1);

    // Register child_process functions
    context.registerGlobalFunction("__edgebox_spawn", nativeSpawn, 4);

    // Basic fetch polyfill using native binding
    const fetch_polyfill =
        \\// EdgeBox fetch() polyfill (HTTP only, HTTPS requires TLS)
        \\globalThis.fetch = async function(url, options = {}) {
        \\    const method = options.method || 'GET';
        \\    const body = options.body || null;
        \\
        \\    // Call native fetch
        \\    const result = globalThis.__edgebox_fetch(url, method, null, body);
        \\
        \\    return {
        \\        ok: result.ok,
        \\        status: result.status,
        \\        headers: result.headers,
        \\        text: async () => result.body,
        \\        json: async () => JSON.parse(result.body),
        \\    };
        \\};
        \\
        \\// Basic console polyfill
        \\if (typeof console === 'undefined') {
        \\    globalThis.console = {
        \\        log: (...args) => print(...args),
        \\        error: (...args) => print('ERROR:', ...args),
        \\        warn: (...args) => print('WARN:', ...args),
        \\        info: (...args) => print('INFO:', ...args),
        \\    };
        \\}
        \\
        \\// Process polyfill
        \\globalThis.process = globalThis.process || {
        \\    env: {},
        \\    cwd: () => '/',
        \\    exit: (code) => std.exit(code || 0),
        \\    argv: scriptArgs || [],
        \\    platform: 'wasi',
        \\    version: 'v20.0.0',
        \\    stdin: {
        \\        isTTY: globalThis.__edgebox_isatty(0),
        \\        fd: 0,
        \\        read: (size) => globalThis.__edgebox_read_stdin(size || 1024),
        \\    },
        \\    stdout: {
        \\        isTTY: globalThis.__edgebox_isatty(1),
        \\        fd: 1,
        \\        write: (data) => print(data),
        \\    },
        \\    stderr: {
        \\        isTTY: globalThis.__edgebox_isatty(2),
        \\        fd: 2,
        \\        write: (data) => print(data),
        \\    },
        \\};
        \\
        \\// TTY module polyfill (Node.js tty module)
        \\globalThis.tty = {
        \\    isatty: (fd) => globalThis.__edgebox_isatty(fd),
        \\    ReadStream: class ReadStream {
        \\        constructor(fd) { this.fd = fd; this.isTTY = globalThis.__edgebox_isatty(fd); }
        \\    },
        \\    WriteStream: class WriteStream {
        \\        constructor(fd) {
        \\            this.fd = fd;
        \\            this.isTTY = globalThis.__edgebox_isatty(fd);
        \\            const size = globalThis.__edgebox_get_terminal_size();
        \\            this.rows = size.rows;
        \\            this.columns = size.cols;
        \\        }
        \\        getWindowSize() { return [this.columns, this.rows]; }
        \\    },
        \\};
        \\
        \\// child_process polyfill (Node.js child_process module)
        \\// Note: Requires WasmEdge process plugin (--enable-process)
        \\globalThis.child_process = {
        \\    // Synchronous spawn - matches Node.js spawnSync API
        \\    spawnSync: function(command, args = [], options = {}) {
        \\        const argsArray = Array.isArray(args) ? args : [];
        \\        const stdinData = options.input || null;
        \\        const timeout = options.timeout || 30000;
        \\
        \\        try {
        \\            const result = globalThis.__edgebox_spawn(command, argsArray, stdinData, timeout);
        \\            return {
        \\                status: result.exitCode,
        \\                stdout: result.stdout,
        \\                stderr: result.stderr,
        \\                error: result.exitCode !== 0 ? new Error('Process exited with code ' + result.exitCode) : null,
        \\                signal: null,
        \\            };
        \\        } catch (e) {
        \\            return {
        \\                status: null,
        \\                stdout: '',
        \\                stderr: '',
        \\                error: e,
        \\                signal: null,
        \\            };
        \\        }
        \\    },
        \\
        \\    // Synchronous exec - simpler API for shell commands
        \\    execSync: function(command, options = {}) {
        \\        // Split command into program and args (simple split, doesn't handle quotes)
        \\        const parts = command.trim().split(/\s+/);
        \\        const program = parts[0];
        \\        const args = parts.slice(1);
        \\
        \\        const result = this.spawnSync(program, args, options);
        \\        if (result.error && result.status !== 0) {
        \\            const err = new Error('Command failed: ' + command);
        \\            err.status = result.status;
        \\            err.stderr = result.stderr;
        \\            throw err;
        \\        }
        \\        return result.stdout;
        \\    },
        \\
        \\    // Async spawn - returns a promise (implemented via sync for now)
        \\    spawn: function(command, args = [], options = {}) {
        \\        return {
        \\            stdout: { on: () => {} },
        \\            stderr: { on: () => {} },
        \\            on: (event, callback) => {
        \\                if (event === 'close') {
        \\                    const result = child_process.spawnSync(command, args, options);
        \\                    setTimeout(() => callback(result.status), 0);
        \\                }
        \\            },
        \\        };
        \\    },
        \\};
    ;

    _ = context.eval(fetch_polyfill) catch |err| {
        std.debug.print("Failed to inject polyfills: {}\n", .{err});
        return err;
    };

    // Additional Node.js polyfills (Buffer, path, url, events, etc.)
    const node_polyfills =
        \\// Buffer polyfill (subset of Node.js Buffer)
        \\// Uses manual UTF-8 encoding since TextEncoder may not be available
        \\globalThis.Buffer = class Buffer extends Uint8Array {
        \\    static _encodeUtf8(str) {
        \\        const bytes = [];
        \\        for (let i = 0; i < str.length; i++) {
        \\            let code = str.charCodeAt(i);
        \\            if (code < 0x80) bytes.push(code);
        \\            else if (code < 0x800) { bytes.push(0xC0 | (code >> 6)); bytes.push(0x80 | (code & 0x3F)); }
        \\            else if (code < 0x10000) { bytes.push(0xE0 | (code >> 12)); bytes.push(0x80 | ((code >> 6) & 0x3F)); bytes.push(0x80 | (code & 0x3F)); }
        \\            else { bytes.push(0xF0 | (code >> 18)); bytes.push(0x80 | ((code >> 12) & 0x3F)); bytes.push(0x80 | ((code >> 6) & 0x3F)); bytes.push(0x80 | (code & 0x3F)); }
        \\        }
        \\        return bytes;
        \\    }
        \\    static _decodeUtf8(bytes) {
        \\        let str = '';
        \\        for (let i = 0; i < bytes.length; ) {
        \\            const b = bytes[i++];
        \\            if (b < 0x80) str += String.fromCharCode(b);
        \\            else if (b < 0xE0) str += String.fromCharCode(((b & 0x1F) << 6) | (bytes[i++] & 0x3F));
        \\            else if (b < 0xF0) str += String.fromCharCode(((b & 0x0F) << 12) | ((bytes[i++] & 0x3F) << 6) | (bytes[i++] & 0x3F));
        \\            else { i += 3; str += '?'; } // 4-byte sequences simplified
        \\        }
        \\        return str;
        \\    }
        \\    static from(data, encoding = 'utf8') {
        \\        if (typeof data === 'string') return new Buffer(Buffer._encodeUtf8(data));
        \\        if (Array.isArray(data)) return new Buffer(data);
        \\        if (data instanceof ArrayBuffer) return new Buffer(data);
        \\        if (data instanceof Uint8Array) return new Buffer(data);
        \\        throw new TypeError('Invalid data type for Buffer.from');
        \\    }
        \\    static alloc(size, fill = 0) {
        \\        const buf = new Buffer(size);
        \\        if (fill !== 0) buf.fill(fill);
        \\        return buf;
        \\    }
        \\    static allocUnsafe(size) { return new Buffer(size); }
        \\    static isBuffer(obj) { return obj instanceof Buffer; }
        \\    static concat(list, totalLength) {
        \\        const total = totalLength || list.reduce((a, b) => a + b.length, 0);
        \\        const result = new Buffer(total);
        \\        let offset = 0;
        \\        for (const buf of list) { result.set(buf, offset); offset += buf.length; }
        \\        return result;
        \\    }
        \\    toString(encoding = 'utf8') { return Buffer._decodeUtf8(this); }
        \\    write(string, offset = 0, length, encoding = 'utf8') {
        \\        const bytes = Buffer._encodeUtf8(string);
        \\        const len = Math.min(bytes.length, length || this.length - offset);
        \\        for (let i = 0; i < len; i++) this[offset + i] = bytes[i];
        \\        return len;
        \\    }
        \\    slice(start, end) { return new Buffer(this.subarray(start, end)); }
        \\    copy(target, targetStart = 0, sourceStart = 0, sourceEnd = this.length) {
        \\        target.set(this.subarray(sourceStart, sourceEnd), targetStart);
        \\        return sourceEnd - sourceStart;
        \\    }
        \\};
        \\
        \\// path polyfill (Node.js path module)
        \\globalThis.path = {
        \\    sep: '/',
        \\    delimiter: ':',
        \\    basename(p, ext) {
        \\        const base = p.split('/').pop() || '';
        \\        if (ext && base.endsWith(ext)) return base.slice(0, -ext.length);
        \\        return base;
        \\    },
        \\    dirname(p) {
        \\        const parts = p.split('/');
        \\        parts.pop();
        \\        return parts.join('/') || '/';
        \\    },
        \\    extname(p) {
        \\        const base = path.basename(p);
        \\        const idx = base.lastIndexOf('.');
        \\        return idx > 0 ? base.slice(idx) : '';
        \\    },
        \\    join(...parts) {
        \\        return parts.join('/').replace(/\/+/g, '/');
        \\    },
        \\    resolve(...parts) {
        \\        let resolved = '';
        \\        for (const p of parts) {
        \\            if (p.startsWith('/')) resolved = p;
        \\            else resolved = resolved + '/' + p;
        \\        }
        \\        return path.normalize(resolved || '/');
        \\    },
        \\    normalize(p) {
        \\        const parts = p.split('/').filter(Boolean);
        \\        const result = [];
        \\        for (const part of parts) {
        \\            if (part === '..') result.pop();
        \\            else if (part !== '.') result.push(part);
        \\        }
        \\        return (p.startsWith('/') ? '/' : '') + result.join('/');
        \\    },
        \\    isAbsolute(p) { return p.startsWith('/'); },
        \\    relative(from, to) {
        \\        const fromParts = path.normalize(from).split('/').filter(Boolean);
        \\        const toParts = path.normalize(to).split('/').filter(Boolean);
        \\        let i = 0;
        \\        while (i < fromParts.length && i < toParts.length && fromParts[i] === toParts[i]) i++;
        \\        return [...Array(fromParts.length - i).fill('..'), ...toParts.slice(i)].join('/');
        \\    },
        \\    parse(p) {
        \\        return {
        \\            root: p.startsWith('/') ? '/' : '',
        \\            dir: path.dirname(p),
        \\            base: path.basename(p),
        \\            ext: path.extname(p),
        \\            name: path.basename(p, path.extname(p)),
        \\        };
        \\    },
        \\    format(obj) {
        \\        return (obj.dir || obj.root || '') + '/' + (obj.base || obj.name + (obj.ext || ''));
        \\    },
        \\};
        \\
        \\// url polyfill (URL is built into QuickJS ES2020+)
        \\globalThis.url = {
        \\    URL: globalThis.URL,
        \\    URLSearchParams: globalThis.URLSearchParams,
        \\    parse(urlString) {
        \\        try {
        \\            const u = new URL(urlString);
        \\            return {
        \\                protocol: u.protocol,
        \\                hostname: u.hostname,
        \\                host: u.host,
        \\                port: u.port,
        \\                pathname: u.pathname,
        \\                search: u.search,
        \\                hash: u.hash,
        \\                href: u.href,
        \\            };
        \\        } catch { return null; }
        \\    },
        \\    format(urlObj) {
        \\        return urlObj.href || (urlObj.protocol + '//' + urlObj.host + urlObj.pathname);
        \\    },
        \\};
        \\
        \\// EventEmitter polyfill (Node.js events module)
        \\globalThis.EventEmitter = class EventEmitter {
        \\    constructor() { this._events = {}; }
        \\    on(event, listener) {
        \\        (this._events[event] = this._events[event] || []).push(listener);
        \\        return this;
        \\    }
        \\    once(event, listener) {
        \\        const wrapper = (...args) => {
        \\            this.off(event, wrapper);
        \\            listener.apply(this, args);
        \\        };
        \\        return this.on(event, wrapper);
        \\    }
        \\    off(event, listener) {
        \\        const listeners = this._events[event];
        \\        if (listeners) {
        \\            const idx = listeners.indexOf(listener);
        \\            if (idx >= 0) listeners.splice(idx, 1);
        \\        }
        \\        return this;
        \\    }
        \\    emit(event, ...args) {
        \\        const listeners = this._events[event];
        \\        if (listeners) listeners.forEach(fn => fn.apply(this, args));
        \\        return !!listeners;
        \\    }
        \\    removeAllListeners(event) {
        \\        if (event) delete this._events[event];
        \\        else this._events = {};
        \\        return this;
        \\    }
        \\    listenerCount(event) { return (this._events[event] || []).length; }
        \\};
        \\globalThis.events = { EventEmitter: globalThis.EventEmitter };
        \\
        \\// util polyfill (subset of Node.js util)
        \\globalThis.util = {
        \\    format(fmt, ...args) {
        \\        let i = 0;
        \\        return String(fmt).replace(/%[sdjoO%]/g, (m) => {
        \\            if (m === '%%') return '%';
        \\            if (i >= args.length) return m;
        \\            const v = args[i++];
        \\            switch (m) {
        \\                case '%s': return String(v);
        \\                case '%d': return Number(v);
        \\                case '%j': case '%o': case '%O': return JSON.stringify(v);
        \\                default: return m;
        \\            }
        \\        });
        \\    },
        \\    inspect(obj, opts) { return JSON.stringify(obj, null, 2); },
        \\    promisify(fn) {
        \\        return (...args) => new Promise((resolve, reject) => {
        \\            fn(...args, (err, result) => err ? reject(err) : resolve(result));
        \\        });
        \\    },
        \\    callbackify(fn) {
        \\        return (...args) => {
        \\            const cb = args.pop();
        \\            fn(...args).then(r => cb(null, r)).catch(e => cb(e));
        \\        };
        \\    },
        \\    inherits(ctor, superCtor) {
        \\        ctor.prototype = Object.create(superCtor.prototype);
        \\        ctor.prototype.constructor = ctor;
        \\    },
        \\    types: {
        \\        isArray: Array.isArray,
        \\        isDate: (v) => v instanceof Date,
        \\        isRegExp: (v) => v instanceof RegExp,
        \\        isPromise: (v) => v instanceof Promise,
        \\    },
        \\};
        \\
        \\// os polyfill (basic subset)
        \\globalThis.os = {
        \\    platform: () => 'wasi',
        \\    arch: () => 'wasm32',
        \\    type: () => 'WASI',
        \\    release: () => '1.0.0',
        \\    hostname: () => 'edgebox',
        \\    homedir: () => process.env.HOME || '/',
        \\    tmpdir: () => '/tmp',
        \\    EOL: '\n',
        \\    cpus: () => [{ model: 'WebAssembly', speed: 0 }],
        \\    totalmem: () => 256 * 1024 * 1024,
        \\    freemem: () => 128 * 1024 * 1024,
        \\};
    ;

    _ = context.eval(node_polyfills) catch |err| {
        std.debug.print("Warning: Failed to inject Node.js polyfills: {}\n", .{err});
    };

    // Additional critical polyfills (timers, encoding, URL, crypto, etc.)
    const critical_polyfills =
        \\// ============================================
        \\// TIMER POLYFILLS
        \\// ============================================
        \\// Note: These are synchronous approximations since WASI doesn't support async timers
        \\// Real timer support would require event loop integration
        \\(function() {
        \\    let _timerId = 1;
        \\    const _timers = new Map();
        \\    const _intervals = new Map();
        \\
        \\    globalThis.setTimeout = function(callback, delay = 0, ...args) {
        \\        const id = _timerId++;
        \\        _timers.set(id, { callback, args, delay });
        \\        // In WASI, we can't do true async, but we register for potential future use
        \\        // For now, if delay is 0, execute immediately via queueMicrotask
        \\        if (delay === 0) {
        \\            queueMicrotask(() => {
        \\                if (_timers.has(id)) {
        \\                    _timers.delete(id);
        \\                    callback(...args);
        \\                }
        \\            });
        \\        }
        \\        return id;
        \\    };
        \\
        \\    globalThis.clearTimeout = function(id) {
        \\        _timers.delete(id);
        \\    };
        \\
        \\    globalThis.setInterval = function(callback, delay = 0, ...args) {
        \\        const id = _timerId++;
        \\        _intervals.set(id, { callback, args, delay });
        \\        return id;
        \\    };
        \\
        \\    globalThis.clearInterval = function(id) {
        \\        _intervals.delete(id);
        \\    };
        \\
        \\    globalThis.setImmediate = function(callback, ...args) {
        \\        return setTimeout(callback, 0, ...args);
        \\    };
        \\
        \\    globalThis.clearImmediate = function(id) {
        \\        clearTimeout(id);
        \\    };
        \\})();
        \\
        \\// ============================================
        \\// TEXT ENCODING POLYFILLS
        \\// ============================================
        \\globalThis.TextEncoder = class TextEncoder {
        \\    constructor(encoding = 'utf-8') {
        \\        this.encoding = encoding;
        \\    }
        \\    encode(str) {
        \\        const bytes = [];
        \\        for (let i = 0; i < str.length; i++) {
        \\            let code = str.charCodeAt(i);
        \\            if (code < 0x80) bytes.push(code);
        \\            else if (code < 0x800) {
        \\                bytes.push(0xC0 | (code >> 6));
        \\                bytes.push(0x80 | (code & 0x3F));
        \\            } else if (code >= 0xD800 && code <= 0xDBFF && i + 1 < str.length) {
        \\                const next = str.charCodeAt(++i);
        \\                if (next >= 0xDC00 && next <= 0xDFFF) {
        \\                    code = ((code - 0xD800) << 10) + (next - 0xDC00) + 0x10000;
        \\                    bytes.push(0xF0 | (code >> 18));
        \\                    bytes.push(0x80 | ((code >> 12) & 0x3F));
        \\                    bytes.push(0x80 | ((code >> 6) & 0x3F));
        \\                    bytes.push(0x80 | (code & 0x3F));
        \\                }
        \\            } else {
        \\                bytes.push(0xE0 | (code >> 12));
        \\                bytes.push(0x80 | ((code >> 6) & 0x3F));
        \\                bytes.push(0x80 | (code & 0x3F));
        \\            }
        \\        }
        \\        return new Uint8Array(bytes);
        \\    }
        \\    encodeInto(str, dest) {
        \\        const encoded = this.encode(str);
        \\        const len = Math.min(encoded.length, dest.length);
        \\        dest.set(encoded.subarray(0, len));
        \\        return { read: str.length, written: len };
        \\    }
        \\};
        \\
        \\globalThis.TextDecoder = class TextDecoder {
        \\    constructor(encoding = 'utf-8') {
        \\        this.encoding = encoding;
        \\        this.fatal = false;
        \\        this.ignoreBOM = false;
        \\    }
        \\    decode(input) {
        \\        const bytes = input instanceof Uint8Array ? input : new Uint8Array(input);
        \\        let str = '';
        \\        for (let i = 0; i < bytes.length; ) {
        \\            const b = bytes[i++];
        \\            if (b < 0x80) str += String.fromCharCode(b);
        \\            else if (b < 0xE0) str += String.fromCharCode(((b & 0x1F) << 6) | (bytes[i++] & 0x3F));
        \\            else if (b < 0xF0) str += String.fromCharCode(((b & 0x0F) << 12) | ((bytes[i++] & 0x3F) << 6) | (bytes[i++] & 0x3F));
        \\            else {
        \\                const code = ((b & 0x07) << 18) | ((bytes[i++] & 0x3F) << 12) | ((bytes[i++] & 0x3F) << 6) | (bytes[i++] & 0x3F);
        \\                if (code > 0xFFFF) {
        \\                    const offset = code - 0x10000;
        \\                    str += String.fromCharCode(0xD800 + (offset >> 10), 0xDC00 + (offset & 0x3FF));
        \\                } else {
        \\                    str += String.fromCharCode(code);
        \\                }
        \\            }
        \\        }
        \\        return str;
        \\    }
        \\};
        \\
        \\// ============================================
        \\// URL POLYFILLS
        \\// ============================================
        \\if (typeof URL === 'undefined') {
        \\    globalThis.URL = class URL {
        \\        constructor(url, base) {
        \\            if (base) url = new URL(base).href.replace(/[^/]*$/, '') + url;
        \\            const match = url.match(/^(\w+):\/\/([^/:]+)(?::(\d+))?(\/[^?#]*)?(\?[^#]*)?(#.*)?$/);
        \\            if (!match) throw new TypeError('Invalid URL: ' + url);
        \\            this.protocol = match[1] + ':';
        \\            this.hostname = match[2];
        \\            this.port = match[3] || '';
        \\            this.pathname = match[4] || '/';
        \\            this.search = match[5] || '';
        \\            this.hash = match[6] || '';
        \\            this.host = this.hostname + (this.port ? ':' + this.port : '');
        \\            this.origin = this.protocol + '//' + this.host;
        \\            this.href = this.origin + this.pathname + this.search + this.hash;
        \\            this.searchParams = new URLSearchParams(this.search);
        \\        }
        \\        toString() { return this.href; }
        \\        toJSON() { return this.href; }
        \\    };
        \\}
        \\
        \\if (typeof URLSearchParams === 'undefined') {
        \\    globalThis.URLSearchParams = class URLSearchParams {
        \\        constructor(init = '') {
        \\            this._params = [];
        \\            if (typeof init === 'string') {
        \\                init = init.replace(/^\?/, '');
        \\                for (const pair of init.split('&')) {
        \\                    const [k, v] = pair.split('=').map(decodeURIComponent);
        \\                    if (k) this._params.push([k, v || '']);
        \\                }
        \\            } else if (init && typeof init === 'object') {
        \\                for (const [k, v] of Object.entries(init)) this._params.push([k, String(v)]);
        \\            }
        \\        }
        \\        append(name, value) { this._params.push([name, String(value)]); }
        \\        delete(name) { this._params = this._params.filter(([k]) => k !== name); }
        \\        get(name) { const p = this._params.find(([k]) => k === name); return p ? p[1] : null; }
        \\        getAll(name) { return this._params.filter(([k]) => k === name).map(([, v]) => v); }
        \\        has(name) { return this._params.some(([k]) => k === name); }
        \\        set(name, value) { this.delete(name); this.append(name, value); }
        \\        toString() { return this._params.map(([k, v]) => encodeURIComponent(k) + '=' + encodeURIComponent(v)).join('&'); }
        \\        *entries() { yield* this._params; }
        \\        *keys() { for (const [k] of this._params) yield k; }
        \\        *values() { for (const [, v] of this._params) yield v; }
        \\        forEach(fn) { this._params.forEach(([k, v]) => fn(v, k, this)); }
        \\        [Symbol.iterator]() { return this.entries(); }
        \\    };
        \\}
        \\
        \\// ============================================
        \\// ABORT CONTROLLER POLYFILL
        \\// ============================================
        \\globalThis.AbortSignal = class AbortSignal extends EventEmitter {
        \\    constructor() {
        \\        super();
        \\        this.aborted = false;
        \\        this.reason = undefined;
        \\    }
        \\    throwIfAborted() {
        \\        if (this.aborted) throw this.reason;
        \\    }
        \\    static abort(reason) {
        \\        const signal = new AbortSignal();
        \\        signal.aborted = true;
        \\        signal.reason = reason || new DOMException('Aborted', 'AbortError');
        \\        return signal;
        \\    }
        \\    static timeout(ms) {
        \\        const signal = new AbortSignal();
        \\        setTimeout(() => {
        \\            signal.aborted = true;
        \\            signal.reason = new DOMException('Timeout', 'TimeoutError');
        \\            signal.emit('abort', signal.reason);
        \\        }, ms);
        \\        return signal;
        \\    }
        \\};
        \\
        \\globalThis.AbortController = class AbortController {
        \\    constructor() {
        \\        this.signal = new AbortSignal();
        \\    }
        \\    abort(reason) {
        \\        if (!this.signal.aborted) {
        \\            this.signal.aborted = true;
        \\            this.signal.reason = reason || new DOMException('Aborted', 'AbortError');
        \\            this.signal.emit('abort', this.signal.reason);
        \\        }
        \\    }
        \\};
        \\
        \\// DOMException polyfill for AbortController
        \\if (typeof DOMException === 'undefined') {
        \\    globalThis.DOMException = class DOMException extends Error {
        \\        constructor(message, name) {
        \\            super(message);
        \\            this.name = name || 'Error';
        \\        }
        \\    };
        \\}
        \\
        \\// ============================================
        \\// CRYPTO POLYFILL
        \\// ============================================
        \\globalThis.crypto = {
        \\    getRandomValues(array) {
        \\        // Use QuickJS std.random or Math.random as fallback
        \\        for (let i = 0; i < array.length; i++) {
        \\            if (array instanceof Uint8Array) array[i] = Math.floor(Math.random() * 256);
        \\            else if (array instanceof Uint16Array) array[i] = Math.floor(Math.random() * 65536);
        \\            else if (array instanceof Uint32Array) array[i] = Math.floor(Math.random() * 4294967296);
        \\            else array[i] = Math.floor(Math.random() * 256);
        \\        }
        \\        return array;
        \\    },
        \\    randomUUID() {
        \\        const bytes = new Uint8Array(16);
        \\        crypto.getRandomValues(bytes);
        \\        bytes[6] = (bytes[6] & 0x0f) | 0x40;
        \\        bytes[8] = (bytes[8] & 0x3f) | 0x80;
        \\        const hex = Array.from(bytes, b => b.toString(16).padStart(2, '0')).join('');
        \\        return hex.slice(0,8) + '-' + hex.slice(8,12) + '-' + hex.slice(12,16) + '-' + hex.slice(16,20) + '-' + hex.slice(20);
        \\    },
        \\    subtle: {
        \\        digest: async (algorithm, data) => {
        \\            throw new Error('crypto.subtle.digest not implemented in WASI');
        \\        }
        \\    }
        \\};
        \\
        \\// ============================================
        \\// REQUIRE / MODULE SYSTEM
        \\// ============================================
        \\(function() {
        \\    const _modules = {
        \\        path: globalThis.path,
        \\        os: globalThis.os,
        \\        events: globalThis.events,
        \\        util: globalThis.util,
        \\        tty: globalThis.tty,
        \\        child_process: globalThis.child_process,
        \\        url: globalThis.url,
        \\        buffer: { Buffer: globalThis.Buffer },
        \\        crypto: globalThis.crypto,
        \\        fs: null, // Will be set below
        \\        http: { request: () => { throw new Error('http.request not yet implemented'); } },
        \\        https: { request: () => { throw new Error('https.request not yet implemented'); } },
        \\        stream: { Readable: class {}, Writable: class {}, Transform: class {}, PassThrough: class {} },
        \\        net: { createConnection: () => { throw new Error('net.createConnection not yet implemented'); } },
        \\        dns: { lookup: () => { throw new Error('dns.lookup not yet implemented'); } },
        \\    };
        \\
        \\    // Basic fs module using std.loadFile/std.saveFile
        \\    _modules.fs = {
        \\        existsSync(path) {
        \\            try { std.loadFile(path); return true; } catch { return false; }
        \\        },
        \\        readFileSync(path, options) {
        \\            const encoding = typeof options === 'string' ? options : options?.encoding;
        \\            try {
        \\                const content = std.loadFile(path);
        \\                if (encoding === 'utf8' || encoding === 'utf-8') return content;
        \\                return Buffer.from(content);
        \\            } catch (e) {
        \\                const err = new Error('ENOENT: no such file or directory, open \'' + path + '\'');
        \\                err.code = 'ENOENT';
        \\                throw err;
        \\            }
        \\        },
        \\        writeFileSync(path, data, options) {
        \\            const content = typeof data === 'string' ? data : data.toString();
        \\            const file = std.open(path, 'w');
        \\            if (!file) throw new Error('ENOENT: cannot open file for writing: ' + path);
        \\            file.puts(content);
        \\            file.close();
        \\        },
        \\        appendFileSync(path, data) {
        \\            const content = typeof data === 'string' ? data : data.toString();
        \\            const file = std.open(path, 'a');
        \\            if (!file) throw new Error('ENOENT: cannot open file for appending: ' + path);
        \\            file.puts(content);
        \\            file.close();
        \\        },
        \\        unlinkSync(path) {
        \\            try { os.remove(path); } catch (e) {
        \\                const err = new Error('ENOENT: no such file or directory: ' + path);
        \\                err.code = 'ENOENT';
        \\                throw err;
        \\            }
        \\        },
        \\        mkdirSync(path, options) {
        \\            const recursive = options?.recursive || false;
        \\            try { os.mkdir(path); } catch (e) {
        \\                if (!recursive) throw e;
        \\            }
        \\        },
        \\        rmdirSync(path) {
        \\            try { os.remove(path); } catch (e) {
        \\                throw new Error('ENOENT: no such directory: ' + path);
        \\            }
        \\        },
        \\        readdirSync(path) {
        \\            try {
        \\                const [entries, err] = os.readdir(path);
        \\                if (err) throw new Error('ENOENT: no such directory: ' + path);
        \\                return entries.filter(e => e !== '.' && e !== '..');
        \\            } catch (e) {
        \\                const err = new Error('ENOENT: no such directory: ' + path);
        \\                err.code = 'ENOENT';
        \\                throw err;
        \\            }
        \\        },
        \\        statSync(path) {
        \\            try {
        \\                const [stat, err] = os.stat(path);
        \\                if (err) throw new Error('ENOENT: ' + path);
        \\                return {
        \\                    isFile: () => (stat.mode & os.S_IFMT) === os.S_IFREG,
        \\                    isDirectory: () => (stat.mode & os.S_IFMT) === os.S_IFDIR,
        \\                    isSymbolicLink: () => (stat.mode & os.S_IFMT) === os.S_IFLNK,
        \\                    size: stat.size,
        \\                    mtime: new Date(stat.mtime * 1000),
        \\                    atime: new Date(stat.atime * 1000),
        \\                    ctime: new Date(stat.ctime * 1000),
        \\                    mode: stat.mode,
        \\                };
        \\            } catch (e) {
        \\                const err = new Error('ENOENT: no such file or directory: ' + path);
        \\                err.code = 'ENOENT';
        \\                throw err;
        \\            }
        \\        },
        \\        lstatSync(path) { return this.statSync(path); },
        \\        realpathSync(path) { return path; }, // Simplified
        \\        copyFileSync(src, dest) {
        \\            const content = this.readFileSync(src);
        \\            this.writeFileSync(dest, content);
        \\        },
        \\        renameSync(oldPath, newPath) {
        \\            try { os.rename(oldPath, newPath); } catch (e) {
        \\                throw new Error('ENOENT: cannot rename: ' + oldPath);
        \\            }
        \\        },
        \\        chmodSync(path, mode) {
        \\            // Not fully supported in WASI
        \\        },
        \\        promises: {
        \\            async readFile(path, options) { return _modules.fs.readFileSync(path, options); },
        \\            async writeFile(path, data, options) { return _modules.fs.writeFileSync(path, data, options); },
        \\            async unlink(path) { return _modules.fs.unlinkSync(path); },
        \\            async mkdir(path, options) { return _modules.fs.mkdirSync(path, options); },
        \\            async rmdir(path) { return _modules.fs.rmdirSync(path); },
        \\            async readdir(path) { return _modules.fs.readdirSync(path); },
        \\            async stat(path) { return _modules.fs.statSync(path); },
        \\            async lstat(path) { return _modules.fs.lstatSync(path); },
        \\            async realpath(path) { return _modules.fs.realpathSync(path); },
        \\            async copyFile(src, dest) { return _modules.fs.copyFileSync(src, dest); },
        \\            async rename(oldPath, newPath) { return _modules.fs.renameSync(oldPath, newPath); },
        \\        },
        \\    };
        \\
        \\    globalThis.require = function(id) {
        \\        if (_modules[id]) return _modules[id];
        \\        throw new Error('Cannot find module \'' + id + '\'');
        \\    };
        \\
        \\    // CommonJS module exports
        \\    globalThis.module = { exports: {} };
        \\    globalThis.exports = globalThis.module.exports;
        \\})();
    ;

    _ = context.eval(critical_polyfills) catch |err| {
        std.debug.print("Warning: Failed to inject critical polyfills: {}\n", .{err});
    };
}

// ============================================================================
// Native Bindings for WASM
// ============================================================================

const qjs = quickjs.c;

/// Get JS undefined value
inline fn jsUndefined() qjs.JSValue {
    // JS_UNDEFINED in QuickJS - use the C macro via eval
    return qjs.JS_UNDEFINED;
}

/// Get JS bool value
inline fn jsBool(val: bool) qjs.JSValue {
    return if (val) qjs.JS_TRUE else qjs.JS_FALSE;
}

/// Get string argument from JS value
fn getStringArg(ctx: ?*qjs.JSContext, val: qjs.JSValue) ?[]const u8 {
    var len: usize = undefined;
    const cstr = qjs.JS_ToCStringLen(ctx, &len, val);
    if (cstr == null) return null;
    return cstr[0..len];
}

/// Free string argument
fn freeStringArg(ctx: ?*qjs.JSContext, str: []const u8) void {
    qjs.JS_FreeCString(ctx, str.ptr);
}

/// Native fetch implementation for WASM
fn nativeFetch(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "fetch requires url argument");

    const url = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "url must be a string");
    defer freeStringArg(ctx, url);

    // Get method (default GET)
    const method = if (argc >= 2) getStringArg(ctx, argv[1]) orelse "GET" else "GET";
    const method_owned = argc >= 2 and getStringArg(ctx, argv[1]) != null;
    defer if (method_owned) freeStringArg(ctx, method);

    // Get body (optional - arg 3)
    const body = if (argc >= 4 and !qjs.JS_IsUndefined(argv[3]) and !qjs.JS_IsNull(argv[3]))
        getStringArg(ctx, argv[3])
    else
        null;
    defer if (body) |b| freeStringArg(ctx, b);

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    // Perform fetch using WASI sockets
    var response = wasm_fetch.jsFetch(allocator, url, method, null, body) catch |err| {
        return switch (err) {
            wasm_fetch.FetchError.InvalidUrl => qjs.JS_ThrowTypeError(ctx, "Invalid URL"),
            wasm_fetch.FetchError.ConnectionFailed => qjs.JS_ThrowInternalError(ctx, "Connection failed"),
            wasm_fetch.FetchError.HostNotFound => qjs.JS_ThrowInternalError(ctx, "Host not found"),
            wasm_fetch.FetchError.Timeout => qjs.JS_ThrowInternalError(ctx, "Request timed out"),
            wasm_fetch.FetchError.InvalidResponse => qjs.JS_ThrowInternalError(ctx, "Invalid HTTP response"),
            wasm_fetch.FetchError.OutOfMemory => qjs.JS_ThrowInternalError(ctx, "Out of memory"),
            wasm_fetch.FetchError.TlsNotSupported => qjs.JS_ThrowInternalError(ctx, "HTTPS not supported yet (use HTTP)"),
        };
    };
    defer response.deinit();

    // Create response object
    const obj = qjs.JS_NewObject(ctx);

    // Set status
    _ = qjs.JS_SetPropertyStr(ctx, obj, "status", qjs.JS_NewInt32(ctx, @intCast(response.status)));

    // Set ok (status 200-299)
    _ = qjs.JS_SetPropertyStr(ctx, obj, "ok", jsBool(response.status >= 200 and response.status < 300));

    // Set body as string
    _ = qjs.JS_SetPropertyStr(ctx, obj, "body", qjs.JS_NewStringLen(ctx, response.body.ptr, response.body.len));

    // Set headers as object
    const headers_obj = qjs.JS_NewObject(ctx);
    for (response.headers.items) |h| {
        // Need null-terminated key for JS_SetPropertyStr
        var key_buf: [256]u8 = undefined;
        if (h.name.len < key_buf.len) {
            @memcpy(key_buf[0..h.name.len], h.name);
            key_buf[h.name.len] = 0;
            _ = qjs.JS_SetPropertyStr(
                ctx,
                headers_obj,
                &key_buf,
                qjs.JS_NewStringLen(ctx, h.value.ptr, h.value.len),
            );
        }
    }
    _ = qjs.JS_SetPropertyStr(ctx, obj, "headers", headers_obj);

    return obj;
}

/// Native isatty implementation
fn nativeIsatty(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return jsBool(false);

    var fd: i32 = 0;
    if (qjs.JS_ToInt32(ctx, &fd, argv[0]) < 0) {
        return jsBool(false);
    }

    const is_tty = wasi_tty.isatty(fd);
    return jsBool(is_tty);
}

/// Native get terminal size implementation
fn nativeGetTerminalSize(ctx: ?*qjs.JSContext, _: qjs.JSValue, _: c_int, _: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const size = wasi_tty.getTerminalSize() orelse {
        // Return default size
        const obj = qjs.JS_NewObject(ctx);
        _ = qjs.JS_SetPropertyStr(ctx, obj, "rows", qjs.JS_NewInt32(ctx, 24));
        _ = qjs.JS_SetPropertyStr(ctx, obj, "cols", qjs.JS_NewInt32(ctx, 80));
        return obj;
    };

    const obj = qjs.JS_NewObject(ctx);
    _ = qjs.JS_SetPropertyStr(ctx, obj, "rows", qjs.JS_NewInt32(ctx, size.rows));
    _ = qjs.JS_SetPropertyStr(ctx, obj, "cols", qjs.JS_NewInt32(ctx, size.cols));
    return obj;
}

/// Native read stdin implementation
fn nativeReadStdin(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    var max_size: i32 = 1024;
    if (argc >= 1) {
        _ = qjs.JS_ToInt32(ctx, &max_size, argv[0]);
    }
    if (max_size <= 0) max_size = 1024;

    const line = wasi_tty.readLine(allocator, @intCast(max_size)) catch |err| {
        return qjs.JS_ThrowInternalError(ctx, "read error: %d", @intFromError(err));
    } orelse {
        return qjs.JS_NULL;
    };
    defer allocator.free(line);

    return qjs.JS_NewStringLen(ctx, line.ptr, line.len);
}

/// Native spawn implementation for child_process
/// Args: command (string), args (array), stdin (string|null), timeout (number)
fn nativeSpawn(ctx: ?*qjs.JSContext, _: qjs.JSValue, argc: c_int, argv: [*c]qjs.JSValue) callconv(.c) qjs.JSValue {
    if (argc < 1) return qjs.JS_ThrowTypeError(ctx, "spawn requires command argument");

    const allocator = global_allocator orelse
        return qjs.JS_ThrowInternalError(ctx, "allocator not initialized");

    // Get command
    const command = getStringArg(ctx, argv[0]) orelse
        return qjs.JS_ThrowTypeError(ctx, "command must be a string");
    defer freeStringArg(ctx, command);

    // Build command
    var cmd = wasi_process.Command.init(allocator, command);
    defer cmd.deinit();

    // Parse args array (arg 1)
    if (argc >= 2 and qjs.JS_IsArray(argv[1])) {
        const arr_len_val = qjs.JS_GetPropertyStr(ctx, argv[1], "length");
        var arr_len: i32 = 0;
        _ = qjs.JS_ToInt32(ctx, &arr_len, arr_len_val);
        qjs.JS_FreeValue(ctx, arr_len_val);

        var i: u32 = 0;
        while (i < @as(u32, @intCast(arr_len))) : (i += 1) {
            const elem = qjs.JS_GetPropertyUint32(ctx, argv[1], i);
            defer qjs.JS_FreeValue(ctx, elem);

            if (getStringArg(ctx, elem)) |arg_str| {
                // Need to copy because arg_str is freed when elem is freed
                const arg_copy = allocator.dupe(u8, arg_str) catch {
                    return qjs.JS_ThrowInternalError(ctx, "out of memory");
                };
                freeStringArg(ctx, arg_str);
                _ = cmd.arg(arg_copy) catch {
                    allocator.free(arg_copy);
                    return qjs.JS_ThrowInternalError(ctx, "out of memory");
                };
            }
        }
    }

    // Set stdin data (arg 2)
    if (argc >= 3 and !qjs.JS_IsNull(argv[2]) and !qjs.JS_IsUndefined(argv[2])) {
        if (getStringArg(ctx, argv[2])) |stdin_data| {
            _ = cmd.setStdin(stdin_data);
            // Note: don't free stdin_data until after command runs
        }
    }

    // Set timeout (arg 3)
    if (argc >= 4) {
        var timeout: i32 = 30000;
        _ = qjs.JS_ToInt32(ctx, &timeout, argv[3]);
        if (timeout > 0) {
            _ = cmd.setTimeout(@intCast(timeout));
        }
    }

    // Run the command
    var result = cmd.output() catch |err| {
        return switch (err) {
            wasi_process.ProcessError.CommandFailed => qjs.JS_ThrowInternalError(ctx, "Command failed to execute (WasmEdge process plugin not enabled?)"),
            wasi_process.ProcessError.TimedOut => qjs.JS_ThrowInternalError(ctx, "Command timed out"),
            wasi_process.ProcessError.OutOfMemory => qjs.JS_ThrowInternalError(ctx, "Out of memory"),
            wasi_process.ProcessError.InvalidCommand => qjs.JS_ThrowTypeError(ctx, "Invalid command"),
        };
    };
    defer result.deinit();

    // Create result object
    const obj = qjs.JS_NewObject(ctx);

    // Set exitCode
    _ = qjs.JS_SetPropertyStr(ctx, obj, "exitCode", qjs.JS_NewInt32(ctx, result.exit_code));

    // Set stdout
    if (result.stdout.len > 0) {
        _ = qjs.JS_SetPropertyStr(ctx, obj, "stdout", qjs.JS_NewStringLen(ctx, result.stdout.ptr, result.stdout.len));
    } else {
        _ = qjs.JS_SetPropertyStr(ctx, obj, "stdout", qjs.JS_NewString(ctx, ""));
    }

    // Set stderr
    if (result.stderr.len > 0) {
        _ = qjs.JS_SetPropertyStr(ctx, obj, "stderr", qjs.JS_NewStringLen(ctx, result.stderr.ptr, result.stderr.len));
    } else {
        _ = qjs.JS_SetPropertyStr(ctx, obj, "stderr", qjs.JS_NewString(ctx, ""));
    }

    return obj;
}
