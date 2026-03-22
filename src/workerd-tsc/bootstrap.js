globalThis.process = globalThis.process || {
  argv: ['edgebox'], env: {}, platform: 'linux',
  cwd: function() { return JSON.parse(__edgebox_io_sync('{"op":"cwd"}')).data || '/'; },
  exit: function(code) { __edgebox_io_sync('{"op":"exit","code":' + (code||0) + '}'); },
  stdout: { write: function(s) { __edgebox_write_stdout(String(s)); return true; }, isTTY: false, columns: 80 },
  stderr: { write: function(s) { __edgebox_write_stderr(String(s)); return true; }, isTTY: false },
  versions: { node: '20.0.0' },
  nextTick: function(cb) { queueMicrotask(cb); },
};
if (typeof Buffer === 'undefined') {
  globalThis.Buffer = { from: function(s) { return s; }, isBuffer: function() { return false; }, alloc: function(n) { return new Uint8Array(n); } };
}
// Tell TSC where lib.d.ts files live (resolve from cwd, not hardcoded)
globalThis.__edgebox_ts_lib = process.cwd() + '/node_modules/typescript/lib';
module.exports = {};
