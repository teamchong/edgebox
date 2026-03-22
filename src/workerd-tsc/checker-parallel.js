import './bootstrap.js';
import './typescript.js';
var ts = globalThis.ts || globalThis.module.exports;

function patchSys(projectCwd) {
  function resolvePath(p) { p = String(p); return p.charAt(0) !== '/' ? projectCwd + '/' + p : p; }
  ts.sys.readFile = function(p) { var c = __edgebox_read_file(resolvePath(p)); return c || undefined; };
  ts.sys.fileExists = function(p) { return __edgebox_file_exists(resolvePath(p)) === 1; };
  ts.sys.directoryExists = function(p) { return __edgebox_dir_exists(resolvePath(p)) === 1; };
  ts.sys.getDirectories = function(p) {
    var rp = resolvePath(p); var entries = JSON.parse(__edgebox_readdir(rp));
    return entries.filter(function(e) { return __edgebox_dir_exists(rp + '/' + e) === 1; });
  };
  ts.sys.readDirectory = function(rootDir, extensions, excludes, includes, depth) {
    return ts.matchFiles(rootDir, extensions, excludes, includes, true, projectCwd, depth, function(p) {
      var rp = p || '.'; var json = __edgebox_readdir(rp);
      if (!json || json === '[]') return { files: [], directories: [] };
      var entries = JSON.parse(json); var files = [], dirs = [];
      for (var i = 0; i < entries.length; i++) {
        if (entries[i] === '.' || entries[i] === '..') continue;
        if (__edgebox_dir_exists(rp + '/' + entries[i]) === 1) dirs.push(entries[i]); else files.push(entries[i]);
      }
      return { files: files, directories: dirs };
    }, function(p) { return __edgebox_realpath(p); });
  };
  ts.sys.realpath = function(p) { return __edgebox_realpath(resolvePath(p)); };
  ts.sys.getCurrentDirectory = function() { return projectCwd; };
  var tsLibDir = __edgebox_cwd() + '/node_modules/typescript/lib';
  ts.sys.getExecutingFilePath = function() { return tsLibDir + '/typescript.js'; };
  ts.sys.writeFile = function(p, data) { __edgebox_write_file(resolvePath(p), data); };
  ts.sys.writeOutputIsTTY = function() { return false; };
}

export default {
  async fetch(request) {
    if (!ts || !ts.executeCommandLine) return new Response('TSC not loaded', { status: 500 });

    var body = {};
    try { body = await request.json(); } catch(e) {}
    var projectCwd = body.cwd || '';
    var workerId = body.workerId || 0;
    var workerCount = body.workerCount || 1;

    if (!projectCwd) return new Response('No cwd', { status: 400 });

    patchSys(projectCwd);

    // Capture output
    var output = [];
    ts.sys.write = function(s) { output.push(String(s)); };
    ts.sys.exit = function() {};
    ts.sys.args = ['--noEmit', '-p', projectCwd + '/tsconfig.json'];

    try {
      ts.executeCommandLine(ts.sys, ts.noop, ts.sys.args);
    } catch(e) {
      output.push('error: ' + e.message + '\n');
    }

    // Filter diagnostics to this worker's shard
    if (workerCount > 1) {
      var all = output.join('').split('\n');
      var sharded = [];
      for (var i = 0; i < all.length; i++) {
        if (i % workerCount === workerId) sharded.push(all[i]);
      }
      return new Response(sharded.join('\n'), { headers: { 'Content-Type': 'text/plain' } });
    }

    return new Response(output.join(''), { headers: { 'Content-Type': 'text/plain' } });
  }
};
