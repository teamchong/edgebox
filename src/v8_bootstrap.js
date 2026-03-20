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
      // Fast path: use direct V8 callback (no JSON serialize/parse overhead)
      if (typeof __edgebox_read_file === 'function') {
        var data = __edgebox_read_file(String(path));
        if (data === undefined) { var err = new Error('ENOENT'); err.code = 'ENOENT'; throw err; }
        return (encoding === 'utf8' || encoding === 'utf-8') ? data : Buffer.from(data);
      }
      var r = _ioSync('readFile', { path: String(path) });
      if (!r.ok) { var err = new Error(r.error || 'ENOENT'); err.code = r.code || 'ENOENT'; throw err; }
      return (encoding === 'utf8' || encoding === 'utf-8' || encoding === 'utf8') ? r.data : Buffer.from(r.data);
    },
    writeFileSync: function(path, data) {
      var r = _ioSync('writeFile', { path: String(path), data: String(data) });
      if (!r.ok) { var err = new Error(r.error); throw err; }
    },
    statSync: function(path, options) {
      // Fast path: use fileExists/dirExists callbacks when available
      if (typeof __edgebox_file_exists === 'function') {
        var p = String(path);
        var isFile = !!__edgebox_file_exists(p);
        var isDir = !isFile && !!__edgebox_dir_exists(p);
        if (!isFile && !isDir) {
          if (options && options.throwIfNoEntry === false) return undefined;
          var err = new Error('ENOENT'); err.code = 'ENOENT'; throw err;
        }
        return {
          isFile: function() { return isFile; },
          isDirectory: function() { return isDir; },
          isSymbolicLink: function() { return false; },
          size: 0,
          mtimeMs: Date.now(),
          mtime: new Date()
        };
      }
      var r = _ioSync('stat', { path: String(path) });
      if (!r.ok) {
        if (options && options.throwIfNoEntry === false) return undefined;
        var err = new Error(r.error || 'ENOENT'); err.code = r.code || 'ENOENT'; throw err;
      }
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
      // Fast path: direct V8 callback (no JSON)
      if (typeof __edgebox_file_exists === 'function') {
        return !!__edgebox_file_exists(String(path));
      }
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
      if (typeof __edgebox_realpath === 'function') {
        var result = __edgebox_realpath(String(path));
        if (result === undefined) { var err = new Error('ENOENT'); err.code = 'ENOENT'; throw err; }
        return result;
      }
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
    // fd-based I/O — TSC uses openSync/writeSync/closeSync for output files
    openSync: function(path, flags) {
      var fd = { __path: String(path), __flags: flags, __data: '' };
      _fs.__openFds = _fs.__openFds || {};
      var id = (_fs.__nextFd = (_fs.__nextFd || 100)) + 1;
      _fs.__nextFd = id;
      _fs.__openFds[id] = fd;
      if (flags === 'w' || flags === 'w+') fd.__data = '';
      else {
        try { fd.__data = _fs.readFileSync(path, 'utf8'); } catch(e) { fd.__data = ''; }
      }
      return id;
    },
    writeSync: function(fd, data) {
      if (typeof fd === 'number' && _fs.__openFds && _fs.__openFds[fd]) {
        _fs.__openFds[fd].__data += String(data);
      }
    },
    closeSync: function(fd) {
      if (typeof fd === 'number' && _fs.__openFds && _fs.__openFds[fd]) {
        var info = _fs.__openFds[fd];
        if (info.__flags === 'w' || info.__flags === 'w+') {
          _fs.writeFileSync(info.__path, info.__data);
        }
        delete _fs.__openFds[fd];
      }
    },
    watchFile: function() { return { close: function() {} }; },
    unwatchFile: function() {},
    watch: function() { return { close: function() {}, on: function() { return this; } }; },
    accessSync: function() { /* no-op, assume accessible */ },
    chmodSync: function() {},
    renameSync: function(oldPath, newPath) {
      // TSC uses this for atomic writes
      var content = _fs.readFileSync(oldPath, 'utf8');
      _fs.writeFileSync(newPath, content);
      _fs.unlinkSync(oldPath);
    },
    lstatSync: function(path, options) { return _fs.statSync(path, options); },
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
  _proc.exit = function(code) { _ioSync('exit', { code: code || 0 }); throw new Error('__edgebox_exit__' + (code || 0)); };
  _proc.platform = 'linux';
  _proc.arch = 'x64';
  _proc.version = 'v20.0.0';
  _proc.versions = { node: '20.0.0', v8: '14.6.202.9' };
  _proc.stdout = { write: function(s) { if (typeof __edgebox_write_stdout==='function') __edgebox_write_stdout(String(s)); else _ioSync('writeStdout', { data: String(s) }); return true; }, isTTY: false, columns: 80, _handle: null };
  _proc.stderr = { write: function(s) { if (typeof __edgebox_write_stderr==='function') __edgebox_write_stderr(String(s)); else _ioSync('writeStderr', { data: String(s) }); return true; }, isTTY: false };
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
  _proc.execArgv = [];
  _proc.pid = 1;
  _proc.ppid = 0;
  _proc.title = 'edgebox';
  _proc.execPath = '/usr/bin/edgebox';
  _proc.binding = function() { return {}; };
  _proc.umask = function() { return 0o22; };
  _proc.on = function() { return _proc; };
  _proc.once = function() { return _proc; };
  _proc.off = function() { return _proc; };
  _proc.emit = function() { return false; };
  _proc.removeListener = function() { return _proc; };
  _proc.addListener = function() { return _proc; };
  _proc.listeners = function() { return []; };
  _proc.removeAllListeners = function() { return _proc; };

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
  // Always override — V8 provides a built-in console but its output goes
  // to a ConsoleDelegate (no-op by default). Replace with IO-bridge version.
  {
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

  // ====== Buffer (with base64 support for TSC) ======
  if (!globalThis.Buffer) {
    var _b64chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
    var _b64lookup = new Uint8Array(128);
    for (var i = 0; i < _b64chars.length; i++) _b64lookup[_b64chars.charCodeAt(i)] = i;

    function _b64decode(str) {
      str = str.replace(/[=]+$/, '');
      var len = (str.length * 3) >> 2;
      var bytes = new Uint8Array(len);
      var p = 0;
      for (var i = 0; i < str.length; i += 4) {
        var a = _b64lookup[str.charCodeAt(i)];
        var b = _b64lookup[str.charCodeAt(i + 1)];
        var c = _b64lookup[str.charCodeAt(i + 2)];
        var d = _b64lookup[str.charCodeAt(i + 3)];
        bytes[p++] = (a << 2) | (b >> 4);
        if (i + 2 < str.length) bytes[p++] = ((b & 15) << 4) | (c >> 2);
        if (i + 3 < str.length) bytes[p++] = ((c & 3) << 6) | d;
      }
      return bytes.subarray(0, p);
    }

    function _b64encode(bytes) {
      var result = '';
      for (var i = 0; i < bytes.length; i += 3) {
        var a = bytes[i], b = bytes[i + 1], c = bytes[i + 2];
        result += _b64chars[a >> 2];
        result += _b64chars[((a & 3) << 4) | ((b || 0) >> 4)];
        result += (i + 1 < bytes.length) ? _b64chars[((b & 15) << 2) | ((c || 0) >> 6)] : '=';
        result += (i + 2 < bytes.length) ? _b64chars[c & 63] : '=';
      }
      return result;
    }

    function _makeBuffer(bytes, originalStr) {
      var buf = new Uint8Array(bytes);
      buf.toString = function(enc) {
        if (enc === 'base64') return _b64encode(this);
        if (enc === 'utf8' || enc === 'utf-8' || !enc) {
          var s = '';
          for (var i = 0; i < this.length; i++) s += String.fromCharCode(this[i]);
          return s;
        }
        if (enc === 'hex') {
          var h = '';
          for (var i = 0; i < this.length; i++) h += (this[i] < 16 ? '0' : '') + this[i].toString(16);
          return h;
        }
        return originalStr || '';
      };
      return buf;
    }

    globalThis.Buffer = {
      from: function(data, encoding) {
        if (typeof data === 'string') {
          if (encoding === 'base64') {
            return _makeBuffer(_b64decode(data));
          }
          // utf8 default
          var arr = [];
          for (var i = 0; i < data.length; i++) {
            var code = data.charCodeAt(i);
            if (code < 0x80) { arr.push(code); }
            else if (code < 0x800) { arr.push(0xC0 | (code >> 6), 0x80 | (code & 0x3F)); }
            else { arr.push(0xE0 | (code >> 12), 0x80 | ((code >> 6) & 0x3F), 0x80 | (code & 0x3F)); }
          }
          return _makeBuffer(arr, data);
        }
        if (data instanceof Uint8Array) return _makeBuffer(data);
        if (Array.isArray(data)) return _makeBuffer(data);
        return _makeBuffer(new Uint8Array(0));
      },
      alloc: function(size) { return _makeBuffer(new Uint8Array(size)); },
      isBuffer: function(obj) { return obj instanceof Uint8Array; },
      concat: function(list) {
        var total = 0;
        for (var i = 0; i < list.length; i++) total += list[i].length;
        var result = new Uint8Array(total);
        var offset = 0;
        for (var i = 0; i < list.length; i++) { result.set(list[i], offset); offset += list[i].length; }
        return _makeBuffer(result);
      },
      byteLength: function(str, encoding) {
        if (typeof str === 'string') return str.length;
        return str.length || 0;
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
  function _resolveModule(id, fromDir) {
    // Built-in modules
    if (_modules[id]) return { builtin: id };
    var bareId = id.replace(/^node:/, '');
    if (_modules[bareId]) return { builtin: bareId };

    // Resolve path relative to requiring file's directory
    var resolved;
    if (_path.isAbsolute(id)) {
      resolved = id;
    } else if (id.charAt(0) === '.') {
      // Relative require — resolve from the requiring module's directory
      resolved = _path.resolve(fromDir || _cwd(), id);
    } else {
      // Bare specifier — resolve from CWD
      resolved = _path.resolve(id);
    }
    return { path: resolved };
  }

  function _loadModule(id, fromDir) {
    var r = _resolveModule(id, fromDir);
    if (r.builtin) return _modules[r.builtin];

    var resolved = r.path;

    // Check cache
    if (_cache[resolved]) return _cache[resolved].exports;

    // Read file — try as-is, then with .js extension
    var code;
    try {
      code = _fs.readFileSync(resolved, 'utf8');
    } catch(e) {
      try {
        code = _fs.readFileSync(resolved + '.js', 'utf8');
        resolved = resolved + '.js';
      } catch(e2) {
        throw new Error("Cannot find module '" + id + "' from '" + (fromDir || _cwd()) + "'");
      }
    }

    if (_cache[resolved]) return _cache[resolved].exports;

    // Create module
    var mod = { id: resolved, exports: {}, loaded: false };
    _cache[resolved] = mod;

    // Each module gets its own require that resolves relative to its directory
    var modDir = _path.dirname(resolved);
    var modRequire = function(childId) { return _loadModule(childId, modDir); };
    modRequire.resolve = function(childId) {
      var cr = _resolveModule(childId, modDir);
      return cr.builtin || cr.path;
    };

    // Wrap and execute
    var wrapped = '(function(exports, require, module, __filename, __dirname) {\n' + code + '\n});';
    var fn = (0, eval)(wrapped);
    fn(mod.exports, modRequire, mod, resolved, modDir);
    mod.loaded = true;

    return mod.exports;
  }

  globalThis._loadModule = _loadModule;
  globalThis.require = function(id) { return _loadModule(id, _cwd()); };
  globalThis.require.resolve = function(id) {
    var r = _resolveModule(id, _cwd());
    return r.builtin || r.path;
  };
  globalThis.module = { exports: {} };
  globalThis.exports = globalThis.module.exports;

  // ====== Global aliases ======
  if (typeof globalThis.global === 'undefined') globalThis.global = globalThis;

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

  // ====== edgebox.parallel — true multi-core JS execution ======
  // Dispatches independent functions to separate V8 isolates on OS threads.
  // Usage:
  //   const results = edgebox.parallel([fn1, fn2, fn3]);
  //   // Each function runs on a separate core — true parallelism
  //
  // Also usable as Promise-compatible:
  //   const results = await edgebox.parallelAsync([fn1, fn2, fn3]);
  if (typeof __edgebox_parallel === 'function') {
    var edgebox = globalThis.edgebox || {};
    edgebox.parallel = function(fns) {
      if (!Array.isArray(fns) || fns.length === 0) return [];
      var codes = fns.map(function(fn) {
        if (typeof fn === 'function') return 'return (' + fn.toString() + ')()';
        if (typeof fn === 'string') return fn;
        return 'return null';
      });
      var json = __edgebox_parallel(JSON.stringify(codes));
      return json ? JSON.parse(json) : [];
    };
    edgebox.parallelAsync = function(fns) {
      return Promise.resolve(edgebox.parallel(fns));
    };

    // edgebox.map — parallel map over arrays using multi-core
    // Splits array into chunks, processes each on a separate V8 isolate
    edgebox.map = function(arr, fn, chunkSize) {
      if (!Array.isArray(arr) || arr.length === 0) return [];
      var cpus = 4;
      var size = chunkSize || Math.ceil(arr.length / cpus);
      var chunks = [];
      for (var i = 0; i < arr.length; i += size) {
        chunks.push(arr.slice(i, i + size));
      }
      var fnStr = fn.toString();
      var codes = chunks.map(function(chunk) {
        return 'var fn = ' + fnStr + '; var arr = ' + JSON.stringify(chunk) + '; return arr.map(fn)';
      });
      var json = __edgebox_parallel(JSON.stringify(codes));
      if (!json) return arr.map(fn);
      var results = JSON.parse(json);
      var flat = [];
      for (var j = 0; j < results.length; j++) {
        if (Array.isArray(results[j])) {
          for (var k = 0; k < results[j].length; k++) flat.push(results[j][k]);
        }
      }
      return flat;
    };

    // edgebox.reduce — parallel reduce with combiner
    edgebox.reduce = function(arr, fn, initial, combiner) {
      if (!Array.isArray(arr) || arr.length === 0) return initial;
      var cpus = 4;
      var size = Math.ceil(arr.length / cpus);
      var chunks = [];
      for (var i = 0; i < arr.length; i += size) {
        chunks.push(arr.slice(i, i + size));
      }
      var fnStr = fn.toString();
      var initStr = JSON.stringify(initial);
      var codes = chunks.map(function(chunk) {
        return 'var fn = ' + fnStr + '; var arr = ' + JSON.stringify(chunk) + '; return arr.reduce(fn, ' + initStr + ')';
      });
      var json = __edgebox_parallel(JSON.stringify(codes));
      if (!json) return arr.reduce(fn, initial);
      var results = JSON.parse(json);
      var comb = combiner || fn;
      var result = results[0];
      for (var j = 1; j < results.length; j++) result = comb(result, results[j]);
      return result;
    };

    globalThis.edgebox = edgebox;
  }

})(globalThis);
