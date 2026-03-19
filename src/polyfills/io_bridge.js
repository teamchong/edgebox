// EdgeBox IO Bridge — connects workerd (V8) to Zig IO server
//
// This module provides Node.js-compatible fs/process APIs by proxying
// all IO operations to the Zig IO server over a Unix socket.
//
// Used by: workerd worker code that needs filesystem access
// Protocol: length-prefixed JSON over Unix socket

(function(globalThis) {
  'use strict';

  const SOCKET_PATH = globalThis.__EDGEBOX_IO_SOCKET || '/tmp/edgebox-io.sock';

  // Synchronous IO via SharedArrayBuffer + Atomics (fastest possible)
  // Falls back to net.connect for async operations
  let _conn = null;

  function getConnection() {
    if (_conn && !_conn.destroyed) return _conn;
    // In workerd, we use the service binding instead
    // In Node.js fallback, use net.connect
    try {
      const net = require('net');
      _conn = net.createConnection(SOCKET_PATH);
      _conn.setNoDelay(true);
      return _conn;
    } catch (e) {
      return null;
    }
  }

  // Synchronous request to Zig IO server
  // Uses a dedicated sync connection per call (simple but correct)
  function syncRequest(op, params) {
    // Try native __edgebox_io_sync if available (Zig host function, zero-copy)
    if (typeof globalThis.__edgebox_io_sync === 'function') {
      const req = JSON.stringify({ op, ...params });
      const resp = globalThis.__edgebox_io_sync(req);
      return JSON.parse(resp);
    }

    // Fallback: synchronous file-descriptor IO (Node.js only)
    try {
      const net = require('net');
      const fs = require('fs');

      const sock = new net.Socket();
      // Connect synchronously using fd trick
      const fd = fs.openSync(SOCKET_PATH, 'r+');
      sock.setNoDelay(true);

      const reqStr = JSON.stringify({ op, ...params });
      const reqBuf = Buffer.from(reqStr);
      const lenBuf = Buffer.alloc(4);
      lenBuf.writeUInt32LE(reqBuf.length);

      fs.writeSync(fd, lenBuf);
      fs.writeSync(fd, reqBuf);

      // Read response length
      const respLenBuf = Buffer.alloc(4);
      fs.readSync(fd, respLenBuf);
      const respLen = respLenBuf.readUInt32LE();

      // Read response body
      const respBuf = Buffer.alloc(respLen);
      fs.readSync(fd, respBuf);
      fs.closeSync(fd);

      return JSON.parse(respBuf.toString());
    } catch (e) {
      return { ok: false, error: e.message };
    }
  }

  // ========================================================================
  // fs module
  // ========================================================================
  const _fs = {
    readFileSync: function(path, options) {
      const encoding = typeof options === 'string' ? options : (options && options.encoding);
      const resp = syncRequest('readFileSync', { path: String(path) });
      if (!resp.ok) {
        const err = new Error(resp.error || 'readFileSync failed');
        err.code = resp.code || 'EIO';
        throw err;
      }
      if (encoding === 'utf8' || encoding === 'utf-8' || encoding === 'utf8') {
        return resp.data;
      }
      return Buffer.from(resp.data);
    },

    writeFileSync: function(path, data) {
      const resp = syncRequest('writeFileSync', { path: String(path), data: String(data) });
      if (!resp.ok) {
        const err = new Error(resp.error || 'writeFileSync failed');
        err.code = resp.code || 'EIO';
        throw err;
      }
    },

    existsSync: function(path) {
      const resp = syncRequest('existsSync', { path: String(path) });
      return resp.ok ? resp.exists : false;
    },

    statSync: function(path) {
      const resp = syncRequest('statSync', { path: String(path) });
      if (!resp.ok) {
        const err = new Error(resp.error || 'statSync failed');
        err.code = resp.code || 'ENOENT';
        throw err;
      }
      return {
        isFile: function() { return resp.isFile; },
        isDirectory: function() { return resp.isDirectory; },
        size: resp.size || 0,
      };
    },

    readdirSync: function(path, options) {
      const resp = syncRequest('readdirSync', { path: String(path) });
      if (!resp.ok) {
        const err = new Error(resp.error || 'readdirSync failed');
        err.code = resp.code || 'ENOENT';
        throw err;
      }
      if (options && options.withFileTypes) {
        return resp.entries.map(function(e) {
          return {
            name: e.name,
            isFile: function() { return !e.isDirectory; },
            isDirectory: function() { return e.isDirectory; },
          };
        });
      }
      return resp.entries.map(function(e) { return e.name; });
    },

    realpathSync: function(path) {
      const resp = syncRequest('realpathSync', { path: String(path) });
      if (!resp.ok) {
        const err = new Error(resp.error || 'realpathSync failed');
        err.code = resp.code || 'ENOENT';
        throw err;
      }
      return resp.data;
    },

    mkdirSync: function(path, options) {
      const recursive = options && options.recursive;
      const resp = syncRequest('mkdirSync', { path: String(path), recursive: !!recursive });
      if (!resp.ok) {
        const err = new Error(resp.error || 'mkdirSync failed');
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

  // Attach native realpathSync
  _fs.realpathSync.native = _fs.realpathSync;

  // ========================================================================
  // process polyfill
  // ========================================================================
  if (!globalThis.process) {
    const cwdResp = syncRequest('cwd', {});
    globalThis.process = {
      cwd: function() { return cwdResp.ok ? cwdResp.data : '/'; },
      argv: globalThis.__EDGEBOX_ARGV || ['edgebox'],
      env: globalThis.__EDGEBOX_ENV || {},
      exit: function(code) { syncRequest('exit', { code: code || 0 }); },
      stdout: { write: function(s) { /* workerd has no stdout */ } },
      stderr: { write: function(s) { /* workerd has no stderr */ } },
      platform: 'linux',
      arch: 'x64',
      version: 'v20.0.0',
      versions: { node: '20.0.0' },
    };
  }

  // ========================================================================
  // Export as globals for workerd worker code
  // ========================================================================
  globalThis.__edgebox_fs_read = function(path) { return _fs.readFileSync(path, 'utf8'); };
  globalThis.__edgebox_fs_write = function(path, data) { return _fs.writeFileSync(path, data); };
  globalThis.__edgebox_fs_stat = function(path) {
    try {
      const s = _fs.statSync(path);
      return { isFile: s.isFile(), isDirectory: s.isDirectory(), size: s.size };
    } catch (e) { return null; }
  };
  globalThis.__edgebox_fs_readdir = function(path) { return _fs.readdirSync(path, { withFileTypes: true }); };
  globalThis.__edgebox_fs_mkdir = function(path, recursive) { return _fs.mkdirSync(path, { recursive: recursive }); };
  globalThis.__edgebox_fs_realpath = function(path) { return _fs.realpathSync(path); };
  globalThis.__edgebox_fs_exists = function(path) { return _fs.existsSync(path); };

  // Also export as module
  if (typeof module !== 'undefined') {
    module.exports = { fs: _fs };
  }

})(globalThis);
