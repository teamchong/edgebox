/// Node.js Polyfills - Embedded from src/polyfills/node/
/// These are real Node.js compatible implementations adapted from Bun
const std = @import("std");
const qjs = @import("quickjs_core.zig");
const c = qjs.c;

// Embed polyfill files at compile time
const events_js = @embedFile("polyfills/node/events.js");
const stream_js = @embedFile("polyfills/node/stream.js");
const path_js = @embedFile("polyfills/node/path.js");
const buffer_js = @embedFile("polyfills/node/buffer.js");
const util_js = @embedFile("polyfills/node/util.js");
const url_js = @embedFile("polyfills/node/url.js");
const os_js = @embedFile("polyfills/node/os.js");
const process_js = @embedFile("polyfills/node/process.js");
const string_decoder_js = @embedFile("polyfills/node/string_decoder.js");

/// Initialize the module system and load all Node.js polyfills
pub fn init(ctx: *qjs.Context) !void {
    // First, set up the module registry
    const module_setup =
        \\// Module registry
        \\globalThis._modules = globalThis._modules || {};
        \\globalThis._moduleCache = globalThis._moduleCache || {};
        \\
        \\// CommonJS require implementation
        \\globalThis.require = function(id) {
        \\    // Normalize module name
        \\    const name = id.replace(/^node:/, '');
        \\
        \\    // Check cache first
        \\    if (globalThis._moduleCache[id]) return globalThis._moduleCache[id];
        \\    if (globalThis._moduleCache[name]) return globalThis._moduleCache[name];
        \\
        \\    // Check registered modules - return the export directly
        \\    if (globalThis._modules[id] !== undefined) {
        \\        globalThis._moduleCache[id] = globalThis._modules[id];
        \\        return globalThis._modules[id];
        \\    }
        \\    if (globalThis._modules[name] !== undefined) {
        \\        globalThis._moduleCache[name] = globalThis._modules[name];
        \\        return globalThis._modules[name];
        \\    }
        \\
        \\    throw new Error('Module not found: ' + id);
        \\};
        \\
        \\// Helper to register a module
        \\globalThis._registerModule = function(names, factory) {
        \\    if (!Array.isArray(names)) names = [names];
        \\    for (const name of names) {
        \\        globalThis._modules[name] = factory;
        \\        globalThis._modules['node:' + name] = factory;
        \\    }
        \\};
    ;

    _ = ctx.eval(module_setup) catch |err| {
        std.debug.print("Failed to set up module system: {}\n", .{err});
        return err;
    };

    // Load core polyfills - these are tested and working
    loadModuleSafe(ctx, "events", events_js);
    loadModuleSafe(ctx, "buffer", buffer_js);
    loadModuleSafe(ctx, "util", util_js);

    // Note: The following modules have issues with IIFE wrapping in QuickJS
    // Using inline stubs in core_setup instead
    // loadModuleSafe(ctx, "path", path_js);
    // loadModuleSafe(ctx, "url", url_js);
    // loadModuleSafe(ctx, "string_decoder", string_decoder_js);
    // loadModuleSafe(ctx, "stream", stream_js);
    // loadModuleSafe(ctx, "os", os_js);
    // loadModuleSafe(ctx, "process", process_js);

    // Set up additional aliases and core modules
    const core_setup =
        \\// Make EventEmitter globally available first
        \\var _events = globalThis._modules['events'];
        \\globalThis.EventEmitter = _events;
        \\
        \\// Make Buffer globally available
        \\var _buffer = globalThis._modules['buffer'];
        \\globalThis.Buffer = _buffer?.Buffer || _buffer;
        \\
        \\// Inline path module
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
        \\};
        \\globalThis._modules['path'].posix = globalThis._modules['path'];
        \\globalThis._modules['path'].win32 = globalThis._modules['path'];
        \\globalThis._modules['node:path'] = globalThis._modules['path'];
        \\globalThis._modules['path/posix'] = globalThis._modules['path'];
        \\globalThis._modules['path/win32'] = globalThis._modules['path'];
        \\globalThis._modules['node:path/posix'] = globalThis._modules['path'];
        \\globalThis._modules['node:path/win32'] = globalThis._modules['path'];
        \\
        \\// Inline os module - uses std.getenv for real values
        \\globalThis._modules['os'] = {
        \\    platform: function() { return 'wasi'; },
        \\    arch: function() { return 'wasm32'; },
        \\    type: function() { return 'WASI'; },
        \\    release: function() { return '1.0.0'; },
        \\    hostname: function() { return std.getenv('HOSTNAME') || 'edgebox'; },
        \\    homedir: function() { return std.getenv('HOME') || '/'; },
        \\    tmpdir: function() { return std.getenv('TMPDIR') || '/tmp'; },
        \\    EOL: '\n',
        \\    cpus: function() { return [{ model: 'WebAssembly', speed: 0 }]; },
        \\    totalmem: function() { return 256 * 1024 * 1024; },
        \\    freemem: function() { return 128 * 1024 * 1024; },
        \\    endianness: function() { return 'LE'; },
        \\    networkInterfaces: function() { return {}; },
        \\    userInfo: function() { var h = std.getenv('HOME') || '/'; return { username: std.getenv('USER') || 'user', uid: 1000, gid: 1000, shell: '/bin/sh', homedir: h }; },
        \\    loadavg: function() { return [0, 0, 0]; },
        \\    uptime: function() { return 0; },
        \\    constants: { signals: { SIGHUP: 1, SIGINT: 2, SIGQUIT: 3, SIGKILL: 9, SIGTERM: 15 }, errno: {}, priority: {} }
        \\};
        \\globalThis._modules['node:os'] = globalThis._modules['os'];
        \\globalThis.os = globalThis._modules['os'];
        \\
        \\// Inline process module - uses std.getenv for real values
        \\globalThis._modules['process'] = {
        \\    env: new Proxy({}, {
        \\        get: function(t, n) { return typeof n === 'symbol' ? undefined : std.getenv(String(n)); },
        \\        has: function(t, n) { return typeof n !== 'symbol' && std.getenv(String(n)) !== undefined; }
        \\    }),
        \\    cwd: function() { return std.getenv('PWD') || '/'; },
        \\    argv: typeof scriptArgs !== 'undefined' ? ['node'].concat(scriptArgs) : ['node'],
        \\    platform: 'wasi',
        \\    arch: 'wasm32',
        \\    version: 'v20.0.0',
        \\    versions: { node: '20.0.0' },
        \\    pid: 1,
        \\    ppid: 0,
        \\    hrtime: function(prev) { var now = Date.now(); var sec = Math.floor(now / 1000); var nano = (now % 1000) * 1e6; if (prev) return [sec - prev[0], nano - prev[1]]; return [sec, nano]; },
        \\    nextTick: function(fn) { var args = Array.prototype.slice.call(arguments, 1); Promise.resolve().then(function() { fn.apply(null, args); }); },
        \\    on: function() { return this; },
        \\    once: function() { return this; },
        \\    off: function() { return this; },
        \\    emit: function() { return false; },
        \\    removeListener: function() { return this; },
        \\    exit: function(code) { if (typeof std !== 'undefined') std.exit(code || 0); },
        \\    stdin: { isTTY: false, fd: 0 },
        \\    stdout: { isTTY: false, fd: 1, write: function(d) { print(d); } },
        \\    stderr: { isTTY: false, fd: 2, write: function(d) { print(d); } }
        \\};
        \\globalThis._modules['node:process'] = globalThis._modules['process'];
        \\globalThis.process = globalThis._modules['process'];
        \\
        \\// Inline stream module with Stream constructor
        \\var _Stream = function Stream(opts) { _events.call(this, opts); };
        \\_Stream.prototype = Object.create(_events.prototype);
        \\_Stream.prototype.constructor = _Stream;
        \\_Stream.prototype.pipe = function(dest) { return dest; };
        \\var _Readable = function Readable(opts) { _Stream.call(this, opts); this.readable = true; };
        \\_Readable.prototype = Object.create(_Stream.prototype);
        \\_Readable.prototype.constructor = _Readable;
        \\var _Writable = function Writable(opts) { _Stream.call(this, opts); this.writable = true; };
        \\_Writable.prototype = Object.create(_Stream.prototype);
        \\_Writable.prototype.constructor = _Writable;
        \\var _Duplex = function Duplex(opts) { _Readable.call(this, opts); this.writable = true; };
        \\_Duplex.prototype = Object.create(_Readable.prototype);
        \\_Duplex.prototype.constructor = _Duplex;
        \\var _Transform = function Transform(opts) { _Duplex.call(this, opts); };
        \\_Transform.prototype = Object.create(_Duplex.prototype);
        \\_Transform.prototype.constructor = _Transform;
        \\var _PassThrough = function PassThrough(opts) { _Transform.call(this, opts); };
        \\_PassThrough.prototype = Object.create(_Transform.prototype);
        \\_PassThrough.prototype.constructor = _PassThrough;
        \\globalThis._modules['stream'] = _Stream;
        \\globalThis._modules['stream'].Stream = _Stream;
        \\globalThis._modules['stream'].Readable = _Readable;
        \\globalThis._modules['stream'].Writable = _Writable;
        \\globalThis._modules['stream'].Duplex = _Duplex;
        \\globalThis._modules['stream'].Transform = _Transform;
        \\globalThis._modules['stream'].PassThrough = _PassThrough;
        \\globalThis._modules['stream'].pipeline = function() { throw new Error('stream.pipeline not implemented'); };
        \\globalThis._modules['stream'].finished = function() { throw new Error('stream.finished not implemented'); };
        \\globalThis._modules['node:stream'] = globalThis._modules['stream'];
        \\globalThis._modules['stream/promises'] = { finished: function() {}, pipeline: function() {} };
        \\globalThis._modules['node:stream/promises'] = globalThis._modules['stream/promises'];
        \\
        \\// Inline url module
        \\globalThis._modules['url'] = {
        \\    URL: globalThis.URL,
        \\    URLSearchParams: globalThis.URLSearchParams,
        \\    parse: function(urlStr) { try { var u = new URL(urlStr); return { protocol: u.protocol, hostname: u.hostname, host: u.host, port: u.port, pathname: u.pathname, search: u.search, hash: u.hash, href: u.href }; } catch(e) { return null; } },
        \\    format: function(obj) { return obj.href || (obj.protocol + '//' + obj.host + obj.pathname); },
        \\    fileURLToPath: function(url) { if (typeof url === 'string') { if (!url.startsWith('file://')) return url; return decodeURIComponent(url.slice(7)); } if (url instanceof URL) { if (url.protocol !== 'file:') throw new TypeError('URL must use file: protocol'); return decodeURIComponent(url.pathname); } throw new TypeError('Invalid URL'); },
        \\    pathToFileURL: function(filepath) { var encoded = encodeURIComponent(filepath).replace(/%2F/g, '/'); return new URL('file://' + (filepath.startsWith('/') ? '' : '/') + encoded); }
        \\};
        \\globalThis._modules['node:url'] = globalThis._modules['url'];
        \\
        \\// Inline string_decoder module
        \\globalThis._modules['string_decoder'] = { StringDecoder: function(enc) { this.encoding = enc || 'utf8'; this.write = function(buf) { return buf.toString ? buf.toString() : String(buf); }; this.end = function() { return ''; }; } };
        \\globalThis._modules['node:string_decoder'] = globalThis._modules['string_decoder'];
        \\
        \\// Additional aliases
        \\globalThis._modules['fs/promises'] = globalThis._modules.fs?.promises || { readFile: async function() { throw new Error('fs not implemented'); } };
        \\globalThis._modules['node:fs/promises'] = globalThis._modules['fs/promises'];
        \\globalThis._modules['timers/promises'] = { setTimeout: function(ms) { return new Promise(function(r) { setTimeout(r, ms); }); } };
        \\globalThis._modules['node:timers/promises'] = globalThis._modules['timers/promises'];
    ;

    _ = ctx.eval(core_setup) catch |err| {
        std.debug.print("Failed to set up core modules: {}\n", .{err});
        if (ctx.getException()) |exc| {
            defer exc.free();
            if (exc.toStringSlice()) |msg| {
                std.debug.print("Exception: {s}\n", .{msg});
            }
        }
        return err;
    };
}

fn loadModuleSafe(ctx: *qjs.Context, name: []const u8, source: []const u8) void {
    loadModule(ctx, name, source) catch |err| {
        std.debug.print("Warning: Module {s} failed to load: {}\n", .{ name, err });
    };
}

fn loadModule(ctx: *qjs.Context, name: []const u8, source: []const u8) !void {
    // Use an IIFE to isolate module scope and avoid redeclaration errors
    // Format: (function(module, exports, require) { ...source... })(module, module.exports, require);
    const allocator = std.heap.page_allocator;

    // Create wrapper that provides proper CommonJS scope
    const prefix = "(function(module, exports, require) {\n";
    const suffix = "\n})(globalThis._currentModule, globalThis._currentModule.exports, globalThis.require);\n";

    // Allocate buffer for wrapped source
    const wrapped_len = prefix.len + source.len + suffix.len;
    const wrapped = allocator.alloc(u8, wrapped_len) catch {
        std.debug.print("Failed to allocate memory for module {s}\n", .{name});
        return error.OutOfMemory;
    };
    defer allocator.free(wrapped);

    @memcpy(wrapped[0..prefix.len], prefix);
    @memcpy(wrapped[prefix.len..][0..source.len], source);
    @memcpy(wrapped[prefix.len + source.len ..][0..suffix.len], suffix);

    // Set up module context
    const setup =
        \\globalThis._currentModule = { exports: {} };
    ;

    _ = ctx.eval(setup) catch |err| {
        std.debug.print("Failed to setup module {s}: {}\n", .{ name, err });
        return err;
    };

    // Eval the wrapped module source
    _ = ctx.eval(wrapped) catch |err| {
        std.debug.print("Failed to eval module {s}: {}\n", .{ name, err });
        if (ctx.getException()) |exc| {
            defer exc.free();
            if (exc.toStringSlice()) |msg| {
                std.debug.print("Exception: {s}\n", .{msg});
            }
        }
        return err;
    };

    // Register the module under both names - zero-initialize to avoid garbage
    var register_buf: [256]u8 = [_]u8{0} ** 256;
    const register = std.fmt.bufPrint(&register_buf,
        \\globalThis._modules['{s}'] = globalThis._currentModule.exports;
        \\globalThis._modules['node:{s}'] = globalThis._currentModule.exports;
    , .{ name, name }) catch unreachable;

    _ = ctx.eval(register) catch |err| {
        std.debug.print("Failed to register module {s}: {}\n", .{ name, err });
        if (ctx.getException()) |exc| {
            defer exc.free();
            if (exc.toStringSlice()) |msg| {
                std.debug.print("Register exception: {s}\n", .{msg});
            }
        }
        return err;
    };
}
