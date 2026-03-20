// EdgeBox IO Bridge — connects workerd (V8) to Zig IO server
//
// This module provides Node.js-compatible fs/process APIs by proxying
// all IO operations to the Zig IO server over a Unix socket.
//
// Used by: workerd worker code that needs filesystem access
// Protocol: length-prefixed JSON over Unix socket
//
// Environment:
//   EDGEBOX_IO_SOCKET — path to Zig IO server Unix socket
//   EDGEBOX_ARGV      — JSON-encoded array of CLI arguments
//   EDGEBOX_CWD       — working directory override

(function(globalThis) {
  'use strict';

  // Resolve socket path: env var → global → default
  var SOCKET_PATH = '/tmp/edgebox-io.sock';
  if (typeof globalThis.__EDGEBOX_IO_SOCKET === 'string') {
    SOCKET_PATH = globalThis.__EDGEBOX_IO_SOCKET;
  } else if (typeof process !== 'undefined' && process.env && process.env.EDGEBOX_IO_SOCKET) {
    SOCKET_PATH = process.env.EDGEBOX_IO_SOCKET;
  }

  // Resolve CLI argv: env var → global → default
  var CLI_ARGV = ['edgebox'];
  if (typeof globalThis.__EDGEBOX_ARGV !== 'undefined') {
    CLI_ARGV = globalThis.__EDGEBOX_ARGV;
  } else if (typeof process !== 'undefined' && process.env && process.env.EDGEBOX_ARGV) {
    try { CLI_ARGV = JSON.parse(process.env.EDGEBOX_ARGV); } catch (e) {}
  }

  // Resolve CWD: env var → global → query IO server
  var CACHED_CWD = null;
  if (typeof globalThis.__EDGEBOX_CWD === 'string') {
    CACHED_CWD = globalThis.__EDGEBOX_CWD;
  } else if (typeof process !== 'undefined' && process.env && process.env.EDGEBOX_CWD) {
    CACHED_CWD = process.env.EDGEBOX_CWD;
  }

  // Synchronous request to Zig IO server
  function syncRequest(op, params) {
    // Path 1: Native Zig host function (zero-copy, fastest)
    if (typeof globalThis.__edgebox_io_sync === 'function') {
      var req = JSON.stringify(Object.assign({ op: op }, params || {}));
      var resp = globalThis.__edgebox_io_sync(req);
      return JSON.parse(resp);
    }

    // Path 2: Synchronous fd-based IO (Node.js / workerd with nodejs_compat)
    try {
      var fs_mod, net_mod;
      try { fs_mod = require('fs'); } catch (e) {}
      try { net_mod = require('net'); } catch (e) {}

      if (fs_mod && fs_mod.openSync && fs_mod.readSync && fs_mod.writeSync) {
        var fd = fs_mod.openSync(SOCKET_PATH, 'r+');
        try {
          var reqStr = JSON.stringify(Object.assign({ op: op }, params || {}));
          var reqBuf = Buffer.from(reqStr);
          var lenBuf = Buffer.alloc(4);
          lenBuf.writeUInt32LE(reqBuf.length);

          fs_mod.writeSync(fd, lenBuf);
          fs_mod.writeSync(fd, reqBuf);

          // Read response length
          var respLenBuf = Buffer.alloc(4);
          var lenRead = 0;
          while (lenRead < 4) {
            var n = fs_mod.readSync(fd, respLenBuf, lenRead, 4 - lenRead, null);
            if (n <= 0) break;
            lenRead += n;
          }
          if (lenRead < 4) return { ok: false, error: 'short read on length' };

          var respLen = respLenBuf.readUInt32LE();
          if (respLen === 0 || respLen > 50 * 1024 * 1024) {
            return { ok: false, error: 'invalid response length: ' + respLen };
          }

          // Read response body
          var respBuf = Buffer.alloc(respLen);
          var bodyRead = 0;
          while (bodyRead < respLen) {
            var m = fs_mod.readSync(fd, respBuf, bodyRead, respLen - bodyRead, null);
            if (m <= 0) break;
            bodyRead += m;
          }

          return JSON.parse(respBuf.toString('utf8', 0, bodyRead));
        } finally {
          fs_mod.closeSync(fd);
        }
      }
    } catch (e) {
      // Fall through to error
    }

    return { ok: false, error: 'no IO transport available (socket: ' + SOCKET_PATH + ')' };
  }

  // ========================================================================
  // fs module — proxies to Zig IO server
  // ========================================================================
  var _fs = {
    readFileSync: function(path, options) {
      var encoding = typeof options === 'string' ? options : (options && options.encoding);
      var resp = syncRequest('readFileSync', { path: String(path) });
      if (!resp.ok) {
        var err = new Error(resp.error || 'readFileSync failed: ' + path);
        err.code = resp.code || 'EIO';
        err.path = path;
        throw err;
      }
      if (encoding === 'utf8' || encoding === 'utf-8') {
        return resp.data;
      }
      return typeof Buffer !== 'undefined' ? Buffer.from(resp.data) : resp.data;
    },

    writeFileSync: function(path, data) {
      var resp = syncRequest('writeFileSync', { path: String(path), data: String(data) });
      if (!resp.ok) {
        var err = new Error(resp.error || 'writeFileSync failed: ' + path);
        err.code = resp.code || 'EIO';
        throw err;
      }
    },

    existsSync: function(path) {
      var resp = syncRequest('existsSync', { path: String(path) });
      return resp.ok ? resp.exists : false;
    },

    statSync: function(path) {
      var resp = syncRequest('statSync', { path: String(path) });
      if (!resp.ok) {
        var err = new Error(resp.error || 'statSync failed: ' + path);
        err.code = resp.code || 'ENOENT';
        throw err;
      }
      return {
        isFile: function() { return resp.isFile; },
        isDirectory: function() { return resp.isDirectory; },
        isSymbolicLink: function() { return false; },
        size: resp.size || 0,
        mtimeMs: resp.mtimeMs || Date.now(),
        mtime: new Date(resp.mtimeMs || Date.now()),
      };
    },

    lstatSync: function(path) {
      // lstat falls back to stat (symlinks not distinguished)
      return _fs.statSync(path);
    },

    readdirSync: function(path, options) {
      var resp = syncRequest('readdirSync', { path: String(path) });
      if (!resp.ok) {
        var err = new Error(resp.error || 'readdirSync failed: ' + path);
        err.code = resp.code || 'ENOENT';
        throw err;
      }
      if (options && options.withFileTypes) {
        return resp.entries.map(function(e) {
          return {
            name: e.name,
            isFile: function() { return !e.isDirectory; },
            isDirectory: function() { return e.isDirectory; },
            isSymbolicLink: function() { return false; },
          };
        });
      }
      return resp.entries.map(function(e) { return e.name; });
    },

    realpathSync: function(path) {
      var resp = syncRequest('realpathSync', { path: String(path) });
      if (!resp.ok) {
        var err = new Error(resp.error || 'realpathSync failed: ' + path);
        err.code = resp.code || 'ENOENT';
        throw err;
      }
      return resp.data;
    },

    mkdirSync: function(path, options) {
      var recursive = options && options.recursive;
      var resp = syncRequest('mkdirSync', { path: String(path), recursive: !!recursive });
      if (!resp.ok) {
        var err = new Error(resp.error || 'mkdirSync failed: ' + path);
        err.code = resp.code || 'EIO';
        throw err;
      }
    },

    unlinkSync: function(path) {
      var resp = syncRequest('unlinkSync', { path: String(path) });
      if (!resp.ok) {
        var err = new Error(resp.error || 'unlinkSync failed');
        err.code = resp.code || 'EIO';
        throw err;
      }
    },

    // Async versions (return Promises)
    readFile: function(path, options) {
      return Promise.resolve(_fs.readFileSync(path, options));
    },
    writeFile: function(path, data) {
      return Promise.resolve(_fs.writeFileSync(path, data));
    },
    stat: function(path) {
      return Promise.resolve(_fs.statSync(path));
    },
    readdir: function(path, options) {
      return Promise.resolve(_fs.readdirSync(path, options));
    },
    realpath: function(path) {
      return Promise.resolve(_fs.realpathSync(path));
    },
    mkdir: function(path, options) {
      return Promise.resolve(_fs.mkdirSync(path, options));
    },
    exists: function(path) {
      return Promise.resolve(_fs.existsSync(path));
    },
  };

  // Attach native realpathSync for Node.js compat
  _fs.realpathSync.native = _fs.realpathSync;

  // ========================================================================
  // process polyfill — stdout/stderr pipe through IO server to real fd 1/2
  // ========================================================================

  // stdout: pipes through IO server to real stdout (fd 1 on Zig side)
  var _stdout = {
    isTTY: false,
    columns: 80,
    rows: 24,
    fd: 1,
    write: function(data) {
      if (data === null || data === undefined) return true;
      var str = String(data);
      if (str.length === 0) return true;
      syncRequest('writeStdout', { data: str });
      return true;
    },
    end: function() {},
    on: function() { return this; },
    once: function() { return this; },
    emit: function() { return false; },
    getWindowSize: function() { return [this.columns, this.rows]; },
    cursorTo: function() {},
    moveCursor: function() {},
    clearLine: function() {},
    clearScreenDown: function() {},
    getColorDepth: function() { return 1; },
    hasColors: function() { return false; },
  };

  // stderr: pipes through IO server to real stderr (fd 2 on Zig side)
  var _stderr = {
    isTTY: false,
    columns: 80,
    rows: 24,
    fd: 2,
    write: function(data) {
      if (data === null || data === undefined) return true;
      var str = String(data);
      if (str.length === 0) return true;
      syncRequest('writeStderr', { data: str });
      return true;
    },
    end: function() {},
    on: function() { return this; },
    once: function() { return this; },
    emit: function() { return false; },
    getWindowSize: function() { return [this.columns, this.rows]; },
    cursorTo: function() {},
    moveCursor: function() {},
    clearLine: function() {},
    clearScreenDown: function() {},
    getColorDepth: function() { return 1; },
    hasColors: function() { return false; },
  };

  // Get cwd from IO server (lazy, cached)
  function getCwd() {
    if (CACHED_CWD !== null) return CACHED_CWD;
    var resp = syncRequest('cwd', {});
    CACHED_CWD = resp.ok ? resp.data : '/';
    return CACHED_CWD;
  }

  // Set up process object
  if (!globalThis.process) globalThis.process = {};
  var _proc = globalThis.process;

  // Only override if not already set by native bindings
  if (!_proc.stdout || !_proc.stdout._isNative) {
    _proc.stdout = _stdout;
  }
  if (!_proc.stderr || !_proc.stderr._isNative) {
    _proc.stderr = _stderr;
  }
  if (!_proc.argv || _proc.argv.length <= 1) {
    _proc.argv = CLI_ARGV;
  }
  if (!_proc.cwd || typeof _proc.cwd !== 'function') {
    _proc.cwd = getCwd;
  }
  if (!_proc.exit) {
    _proc.exit = function(code) {
      syncRequest('exit', { code: code || 0 });
      // In case the IO server doesn't kill us, throw to stop execution
      throw new Error('process.exit(' + (code || 0) + ')');
    };
  }
  if (!_proc.env) _proc.env = {};
  if (!_proc.platform) _proc.platform = 'linux';
  if (!_proc.arch) _proc.arch = 'x64';
  if (!_proc.version) _proc.version = 'v20.0.0';
  if (!_proc.versions) _proc.versions = { node: '20.0.0' };
  if (!_proc.hrtime) {
    _proc.hrtime = function(prev) {
      var now = Date.now();
      var sec = Math.floor(now / 1000);
      var nsec = (now % 1000) * 1000000;
      if (prev) {
        var dsec = sec - prev[0];
        var dnsec = nsec - prev[1];
        if (dnsec < 0) { dsec -= 1; dnsec += 1000000000; }
        return [dsec, dnsec];
      }
      return [sec, nsec];
    };
    _proc.hrtime.bigint = function() {
      return BigInt(Date.now()) * BigInt(1000000);
    };
  }
  if (!_proc.nextTick) {
    _proc.nextTick = function(fn) {
      var args = Array.prototype.slice.call(arguments, 1);
      setTimeout(function() { fn.apply(null, args); }, 0);
    };
  }
  if (!_proc.on) _proc.on = function() { return _proc; };
  if (!_proc.once) _proc.once = function() { return _proc; };
  if (!_proc.off) _proc.off = function() { return _proc; };
  if (!_proc.emit) _proc.emit = function() { return false; };
  if (!_proc.removeListener) _proc.removeListener = function() { return _proc; };
  if (!_proc.removeAllListeners) _proc.removeAllListeners = function() { return _proc; };
  if (!_proc.listeners) _proc.listeners = function() { return []; };
  if (!_proc.listenerCount) _proc.listenerCount = function() { return 0; };
  if (!_proc.addListener) _proc.addListener = _proc.on;
  if (!_proc.pid) _proc.pid = 1;
  if (!_proc.ppid) _proc.ppid = 0;
  if (!_proc.uptime) _proc.uptime = function() { return 0; };
  if (!_proc.memoryUsage) _proc.memoryUsage = function() {
    return { rss: 0, heapTotal: 0, heapUsed: 0, external: 0, arrayBuffers: 0 };
  };
  if (!_proc.cpuUsage) _proc.cpuUsage = function() { return { user: 0, system: 0 }; };

  // ========================================================================
  // Export as globals for worker code (polyfill modules use these)
  // ========================================================================
  globalThis.__edgebox_fs_read = function(path) { return _fs.readFileSync(path, 'utf8'); };
  globalThis.__edgebox_fs_write = function(path, data) { return _fs.writeFileSync(path, data); };
  globalThis.__edgebox_fs_stat = function(path) {
    try {
      var s = _fs.statSync(path);
      return { isFile: s.isFile(), isDirectory: s.isDirectory(), size: s.size };
    } catch (e) { return null; }
  };
  globalThis.__edgebox_fs_readdir = function(path) { return _fs.readdirSync(path, { withFileTypes: true }); };
  globalThis.__edgebox_fs_mkdir = function(path, recursive) { return _fs.mkdirSync(path, { recursive: recursive }); };
  globalThis.__edgebox_fs_realpath = function(path) { return _fs.realpathSync(path); };
  globalThis.__edgebox_fs_exists = function(path) { return _fs.existsSync(path); };
  globalThis.__edgebox_stdout_write = function(s) { _stdout.write(s); };
  globalThis.__edgebox_stderr_write = function(s) { _stderr.write(s); };
  globalThis.__edgebox_cwd = getCwd;

  // Also export as module
  if (typeof module !== 'undefined') {
    module.exports = { fs: _fs };
  }

})(globalThis);
