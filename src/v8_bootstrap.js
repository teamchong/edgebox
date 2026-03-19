// v8_bootstrap.js — Bootstrap script for V8 embed path
//
// Sets up require(), process, console, Buffer, and other Node.js globals
// needed to run CommonJS modules like TSC.
//
// Prerequisites: __edgebox_io_sync must be registered before this runs.

(function(globalThis) {
  'use strict';

  // ====== Module registry ======
  var _modules = {};
  globalThis._modules = _modules;

  // Module cache for user require() calls
  var _cache = {};

  // ====== Path module (minimal) ======
  var sep = '/';
  var _path = {
    sep: sep,
    delimiter: ':',
    join: function() {
      var parts = [];
      for (var i = 0; i < arguments.length; i++) {
        if (arguments[i]) parts.push(arguments[i]);
      }
      return _path.normalize(parts.join('/'));
    },
    normalize: function(p) {
      if (!p) return '.';
      var isAbs = p.charAt(0) === '/';
      var parts = p.split('/');
      var result = [];
      for (var i = 0; i < parts.length; i++) {
        if (parts[i] === '.' || parts[i] === '') continue;
        if (parts[i] === '..') { result.pop(); continue; }
        result.push(parts[i]);
      }
      var out = result.join('/');
      return isAbs ? '/' + out : (out || '.');
    },
    resolve: function() {
      var resolved = '';
      for (var i = arguments.length - 1; i >= 0; i--) {
        resolved = arguments[i] + '/' + resolved;
        if (resolved.charAt(0) === '/') break;
      }
      if (resolved.charAt(0) !== '/') {
        resolved = _cwd() + '/' + resolved;
      }
      return _path.normalize(resolved);
    },
    dirname: function(p) {
      if (!p) return '.';
      var idx = p.lastIndexOf('/');
      return idx <= 0 ? (p.charAt(0) === '/' ? '/' : '.') : p.slice(0, idx);
    },
    basename: function(p, ext) {
      if (!p) return '';
      var base = p.slice(p.lastIndexOf('/') + 1);
      if (ext && base.endsWith(ext)) base = base.slice(0, -ext.length);
      return base;
    },
    extname: function(p) {
      var base = _path.basename(p);
      var idx = base.lastIndexOf('.');
      return idx <= 0 ? '' : base.slice(idx);
    },
    isAbsolute: function(p) { return p && p.charAt(0) === '/'; },
    relative: function(from, to) {
      from = _path.resolve(from).split('/');
      to = _path.resolve(to).split('/');
      var common = 0;
      while (common < from.length && common < to.length && from[common] === to[common]) common++;
      var ups = [];
      for (var i = common; i < from.length; i++) ups.push('..');
      return ups.concat(to.slice(common)).join('/') || '.';
    },
    parse: function(p) {
      return {
        root: _path.isAbsolute(p) ? '/' : '',
        dir: _path.dirname(p),
        base: _path.basename(p),
        ext: _path.extname(p),
        name: _path.basename(p, _path.extname(p))
      };
    },
    format: function(obj) {
      var dir = obj.dir || obj.root || '';
      var base = obj.base || ((obj.name || '') + (obj.ext || ''));
      return dir ? (dir === '/' ? dir + base : dir + '/' + base) : base;
    },
    posix: null, // set below
    win32: null
  };
  _path.posix = _path;
  _modules.path = _path;
  _modules['node:path'] = _path;

  // ====== IO helpers ======
  function _ioSync(op, params) {
    var req = JSON.stringify(Object.assign({ op: op }, params || {}));
    return JSON.parse(globalThis.__edgebox_io_sync(req));
  }

  // ====== CWD ======
  var _cwdCache = null;
  function _cwd() {
    if (!_cwdCache) {
      var r = _ioSync('cwd');
      _cwdCache = r.ok ? r.data : '/';
    }
    return _cwdCache;
  }

  // ====== FS module ======
  var _fs = {
    readFileSync: function(path, options) {
      var encoding = typeof options === 'string' ? options : (options && options.encoding);
      var r = _ioSync('readFile', { path: String(path) });
      if (!r.ok) { var err = new Error(r.error || 'ENOENT'); err.code = r.code || 'ENOENT'; throw err; }
      return (encoding === 'utf8' || encoding === 'utf-8' || encoding === 'utf8') ? r.data : Buffer.from(r.data);
    },
    writeFileSync: function(path, data) {
      var r = _ioSync('writeFile', { path: String(path), data: String(data) });
      if (!r.ok) { var err = new Error(r.error); throw err; }
    },
    statSync: function(path) {
      var r = _ioSync('stat', { path: String(path) });
      if (!r.ok) { var err = new Error(r.error || 'ENOENT'); err.code = r.code || 'ENOENT'; throw err; }
      return {
        isFile: function() { return r.isFile; },
        isDirectory: function() { return r.isDirectory; },
        isSymbolicLink: function() { return false; },
        size: r.size,
        mtimeMs: Date.now(),
        mtime: new Date()
      };
    },
    existsSync: function(path) {
      var r = _ioSync('exists', { path: String(path) });
      return r.ok && r.exists;
    },
    readdirSync: function(path, options) {
      var r = _ioSync('readdir', { path: String(path) });
      if (!r.ok) { var err = new Error(r.error || 'ENOENT'); err.code = r.code || 'ENOENT'; throw err; }
      var withTypes = options && options.withFileTypes;
      return r.entries.map(function(e) {
        if (withTypes) return { name: e.name, isFile: function() { return !e.isDirectory; }, isDirectory: function() { return e.isDirectory; }, isSymbolicLink: function() { return false; } };
        return e.name;
      });
    },
    realpathSync: function(path) {
      var r = _ioSync('realpath', { path: String(path) });
      if (!r.ok) { var err = new Error(r.error); throw err; }
      return r.data;
    },
    mkdirSync: function(path, options) {
      var recursive = options && options.recursive;
      var r = _ioSync('mkdir', { path: String(path), recursive: !!recursive });
      if (!r.ok) { var err = new Error(r.error); throw err; }
    },
    unlinkSync: function(path) {
      var r = _ioSync('unlink', { path: String(path) });
      if (!r.ok) { var err = new Error(r.error); throw err; }
    },
    accessSync: function() { /* no-op, assume accessible */ },
    chmodSync: function() {},
    lstatSync: function(path) { return _fs.statSync(path); },
    readFile: function(path, options, cb) {
      if (typeof options === 'function') { cb = options; options = undefined; }
      try { cb(null, _fs.readFileSync(path, options)); } catch(e) { cb(e); }
    },
    writeFile: function(path, data, options, cb) {
      if (typeof options === 'function') { cb = options; options = undefined; }
      try { _fs.writeFileSync(path, data); cb(null); } catch(e) { cb(e); }
    },
    promises: {}
  };
  _fs.realpathSync.native = _fs.realpathSync;
  _fs.promises.readFile = function(p, o) { try { return Promise.resolve(_fs.readFileSync(p, o)); } catch(e) { return Promise.reject(e); } };
  _fs.promises.writeFile = function(p, d) { try { _fs.writeFileSync(p, d); return Promise.resolve(); } catch(e) { return Promise.reject(e); } };
  _fs.promises.stat = function(p) { try { return Promise.resolve(_fs.statSync(p)); } catch(e) { return Promise.reject(e); } };
  _modules.fs = _fs;
  _modules['node:fs'] = _fs;
  _modules['node:fs/promises'] = _fs.promises;

  // ====== OS module ======
  var _os = {
    platform: function() { return 'linux'; },
    arch: function() { return 'x64'; },
    tmpdir: function() { return '/tmp'; },
    homedir: function() { return '/home'; },
    EOL: '\n',
    cpus: function() { return [{}]; },
    totalmem: function() { return 8 * 1024 * 1024 * 1024; },
    freemem: function() { return 4 * 1024 * 1024 * 1024; },
    type: function() { return 'Linux'; },
    release: function() { return '6.0.0'; },
    hostname: function() { return 'edgebox'; },
    endianness: function() { return 'LE'; }
  };
  _modules.os = _os;
  _modules['node:os'] = _os;

  // ====== Process object ======
  var _proc = globalThis.process || {};
  _proc.argv = ['edgebox'];
  _proc.env = {};
  _proc.cwd = _cwd;
  _proc.exit = function(code) { _ioSync('exit', { code: code || 0 }); };
  _proc.platform = 'linux';
  _proc.arch = 'x64';
  _proc.version = 'v20.0.0';
  _proc.versions = { node: '20.0.0', v8: '14.6.202.9' };
  _proc.stdout = { write: function(s) { _ioSync('writeStdout', { data: String(s) }); }, isTTY: false };
  _proc.stderr = { write: function(s) { _ioSync('writeStderr', { data: String(s) }); }, isTTY: false };
  _proc.stdin = { isTTY: false };
  _proc.hrtime = function(prev) {
    var now = Date.now();
    var sec = Math.floor(now / 1000);
    var nsec = (now % 1000) * 1e6;
    if (prev) { sec -= prev[0]; nsec -= prev[1]; if (nsec < 0) { sec--; nsec += 1e9; } }
    return [sec, nsec];
  };
  _proc.hrtime.bigint = function() { return BigInt(Date.now()) * 1000000n; };
  _proc.nextTick = function(fn) { Promise.resolve().then(fn); };
  _proc.memoryUsage = function() { return { rss: 0, heapTotal: 0, heapUsed: 0, external: 0 }; };
  _proc.cpuUsage = function() { return { user: 0, system: 0 }; };

  // Load actual argv and env from Zig
  try {
    var argvR = _ioSync('argv');
    if (argvR.ok) _proc.argv = argvR.data;
  } catch(e) {}
  try {
    var envR = _ioSync('env');
    if (envR.ok) _proc.env = envR.data;
  } catch(e) {}

  globalThis.process = _proc;
  _modules.process = _proc;
  _modules['node:process'] = _proc;

  // ====== Console ======
  if (!globalThis.console) {
    globalThis.console = {
      log: function() { _proc.stdout.write(Array.prototype.join.call(arguments, ' ') + '\n'); },
      error: function() { _proc.stderr.write(Array.prototype.join.call(arguments, ' ') + '\n'); },
      warn: function() { _proc.stderr.write(Array.prototype.join.call(arguments, ' ') + '\n'); },
      info: function() { _proc.stdout.write(Array.prototype.join.call(arguments, ' ') + '\n'); },
      debug: function() {},
      trace: function() {},
      assert: function(cond) { if (!cond) throw new Error('Assertion failed'); },
      time: function() {},
      timeEnd: function() {},
      timeLog: function() {},
      dir: function(obj) { _proc.stdout.write(JSON.stringify(obj, null, 2) + '\n'); }
    };
  }

  // ====== Buffer (minimal) ======
  if (!globalThis.Buffer) {
    globalThis.Buffer = {
      from: function(data, encoding) {
        if (typeof data === 'string') {
          var arr = [];
          for (var i = 0; i < data.length; i++) arr.push(data.charCodeAt(i) & 0xff);
          var buf = new Uint8Array(arr);
          buf.toString = function(enc) { return data; };
          return buf;
        }
        return new Uint8Array(data);
      },
      alloc: function(size) { return new Uint8Array(size); },
      isBuffer: function(obj) { return obj instanceof Uint8Array; },
      concat: function(list) {
        var total = 0;
        for (var i = 0; i < list.length; i++) total += list[i].length;
        var result = new Uint8Array(total);
        var offset = 0;
        for (var i = 0; i < list.length; i++) { result.set(list[i], offset); offset += list[i].length; }
        return result;
      }
    };
  }

  // ====== Crypto (stub) ======
  _modules.crypto = {
    createHash: function(algo) {
      return {
        _data: '',
        update: function(data) { this._data += data; return this; },
        digest: function(enc) { return this._data.length.toString(16); }
      };
    },
    randomBytes: function(n) { var buf = new Uint8Array(n); for (var i = 0; i < n; i++) buf[i] = Math.random() * 256 | 0; return buf; }
  };
  _modules['node:crypto'] = _modules.crypto;

  // ====== Perf hooks (stub) ======
  _modules.perf_hooks = {
    performance: typeof performance !== 'undefined' ? performance : {
      now: function() { return Date.now(); },
      mark: function() {},
      measure: function() {},
      clearMarks: function() {},
      clearMeasures: function() {}
    },
    PerformanceObserver: function() { this.observe = function() {}; this.disconnect = function() {}; }
  };
  _modules['node:perf_hooks'] = _modules.perf_hooks;
  if (typeof performance === 'undefined') globalThis.performance = _modules.perf_hooks.performance;

  // ====== Inspector (stub) ======
  _modules.inspector = { Session: function() { this.connect = function() {}; this.post = function() {}; this.disconnect = function() {}; } };
  _modules['node:inspector'] = _modules.inspector;

  // ====== Module (stub for TSC) ======
  var _Module = {
    _resolveFilename: function(id) { return id; },
    _cache: {},
    _extensions: {},
    _pathCache: {}
  };
  _modules.module = _Module;
  _modules['node:module'] = _Module;

  // ====== Events (stub) ======
  function EventEmitter() { this._events = {}; }
  EventEmitter.prototype.on = function(e, fn) { (this._events[e] = this._events[e] || []).push(fn); return this; };
  EventEmitter.prototype.emit = function(e) { var fns = this._events[e]; if (fns) for (var i = 0; i < fns.length; i++) fns[i].apply(this, Array.prototype.slice.call(arguments, 1)); return !!fns; };
  EventEmitter.prototype.once = function(e, fn) { var self = this; function f() { self.removeListener(e, f); fn.apply(self, arguments); } this.on(e, f); return this; };
  EventEmitter.prototype.removeListener = function(e, fn) { var fns = this._events[e]; if (fns) this._events[e] = fns.filter(function(f) { return f !== fn; }); return this; };
  EventEmitter.prototype.removeAllListeners = function(e) { if (e) delete this._events[e]; else this._events = {}; return this; };
  EventEmitter.prototype.listeners = function(e) { return this._events[e] || []; };
  EventEmitter.prototype.addListener = EventEmitter.prototype.on;
  EventEmitter.prototype.off = EventEmitter.prototype.removeListener;
  _modules.events = { EventEmitter: EventEmitter, default: EventEmitter };
  _modules['node:events'] = _modules.events;

  // ====== source-map-support (stub) ======
  _modules['source-map-support'] = { install: function() {} };

  // ====== URL (stub) ======
  _modules.url = {
    pathToFileURL: function(p) { return { href: 'file://' + p, pathname: p, toString: function() { return 'file://' + p; } }; },
    fileURLToPath: function(u) { return typeof u === 'string' ? u.replace('file://', '') : u.pathname; },
    URL: typeof URL !== 'undefined' ? URL : function(s) { this.href = s; this.pathname = s; }
  };
  _modules['node:url'] = _modules.url;

  // ====== require() ======
  function _require(id) {
    // Built-in modules
    if (_modules[id]) return _modules[id];

    // Strip node: prefix
    var bareId = id.replace(/^node:/, '');
    if (_modules[bareId]) return _modules[bareId];

    // User module - resolve path
    var resolved = id;
    if (!_path.isAbsolute(id)) {
      // Relative to CWD for now (simple case)
      resolved = _path.resolve(id);
    }

    // Check cache
    if (_cache[resolved]) return _cache[resolved].exports;

    // Read file
    try {
      var code = _fs.readFileSync(resolved, 'utf8');
    } catch(e) {
      // Try with .js extension
      try {
        code = _fs.readFileSync(resolved + '.js', 'utf8');
        resolved = resolved + '.js';
      } catch(e2) {
        throw new Error("Cannot find module '" + id + "'");
      }
    }

    // Create module wrapper
    var mod = { id: resolved, exports: {}, loaded: false };
    _cache[resolved] = mod;

    // Wrap and execute
    var wrapped = '(function(exports, require, module, __filename, __dirname) {\n' + code + '\n});';
    var fn = (0, eval)(wrapped);
    fn(mod.exports, _require, mod, resolved, _path.dirname(resolved));
    mod.loaded = true;

    return mod.exports;
  }

  globalThis.require = _require;
  globalThis.module = { exports: {} };
  globalThis.exports = globalThis.module.exports;

  // ====== Global stubs ======
  if (typeof globalThis.setTimeout === 'undefined') {
    globalThis.setTimeout = function(fn, ms) { fn(); return 0; };
    globalThis.clearTimeout = function() {};
    globalThis.setInterval = function() { return 0; };
    globalThis.clearInterval = function() {};
    globalThis.setImmediate = function(fn) { fn(); return 0; };
    globalThis.clearImmediate = function() {};
    globalThis.queueMicrotask = function(fn) { Promise.resolve().then(fn); };
  }

})(globalThis);
