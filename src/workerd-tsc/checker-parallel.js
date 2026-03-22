import './bootstrap.js';
import './typescript.js';
var ts = globalThis.ts || globalThis.module.exports;

(function() {
  try {
    if (!ts || !ts.createProgram) {
      console.log('[checker] TypeScript not loaded');
      return;
    }

    // Read project config FIRST
    var configJson = __edgebox_read_file('/tmp/edgebox-project-config.json');
    if (!configJson) { console.log('[checker] no config'); return; }
    var config = JSON.parse(configJson);
    var projectCwd = config.cwd;

    // Resolve relative paths against project cwd
    function resolvePath(p) {
      p = String(p);
      if (p.charAt(0) !== '/') p = projectCwd + '/' + p;
      return p;
    }

    // Patch ts.sys with Zig IO (all paths resolved against project cwd)
    if (ts.sys) {
      ts.sys.readFile = function(p) { var c = __edgebox_read_file(resolvePath(p)); return c || undefined; };
      ts.sys.fileExists = function(p) { return __edgebox_file_exists(resolvePath(p)) === 1; };
      ts.sys.directoryExists = function(p) { return __edgebox_dir_exists(resolvePath(p)) === 1; };
      ts.sys.getDirectories = function(p) {
        var rp = resolvePath(p);
        var entries = JSON.parse(__edgebox_readdir(rp));
        return entries.filter(function(e) { return __edgebox_dir_exists(rp + '/' + e) === 1; });
      };
      ts.sys.readDirectory = function(rootDir, extensions, excludes, includes, depth) {
        return ts.matchFiles(rootDir, extensions, excludes, includes, true, projectCwd, depth, function(p) {
          // p is an absolute path from matchFiles
          var rp = p || '.';
          var json = __edgebox_readdir(rp);
          if (!json || json === '[]') return { files: [], directories: [] };
          var entries = JSON.parse(json);
          var files = [], dirs = [];
          for (var i = 0; i < entries.length; i++) {
            if (entries[i] === '.' || entries[i] === '..') continue;
            var full = rp + '/' + entries[i];
            if (__edgebox_dir_exists(full) === 1) dirs.push(entries[i]);
            else files.push(entries[i]);
          }
          return { files: files, directories: dirs };
        }, function(p) { return __edgebox_realpath(p); });
      };
      ts.sys.realpath = function(p) { return __edgebox_realpath(resolvePath(p)); };
      ts.sys.getCurrentDirectory = function() { return projectCwd; };
      // TSC derives lib.d.ts path from getExecutingFilePath — point to typescript/lib/
      var tsLibDir = __edgebox_cwd() + '/node_modules/typescript/lib';
      ts.sys.getExecutingFilePath = function() { return tsLibDir + '/typescript.js'; };
      ts.sys.write = function(s) { __edgebox_write_stdout(String(s)); };
      ts.sys.writeOutputIsTTY = function() { return false; };
      ts.sys.exit = function(code) { __edgebox_exit(code || 0); };
    }

    // Use ts.executeCommandLine — identical behavior to `npx tsc`
    var t0 = Date.now();
    ts.sys.args = ['--noEmit', '-p', projectCwd + '/tsconfig.json'];
    ts.executeCommandLine(ts.sys, ts.noop, ts.sys.args);
    var checkTime = Date.now() - t0;
    console.log('[checker] check completed in ' + checkTime + 'ms');
  } catch(e) {
    console.log('[checker] error: ' + e.message);
    console.log(e.stack || '');
  }
})();

export default {
  fetch() { return new Response('edgebox-checker'); }
};
