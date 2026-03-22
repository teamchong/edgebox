import './bootstrap.js';
import './typescript.js';
var ts = globalThis.ts || globalThis.module.exports;

// Module init: load TypeScript, patch ts.sys (stays warm across requests)
var tsReady = false;
(function() {
  if (!ts || !ts.createProgram) return;

  // Patch ts.sys with Zig IO
  if (ts.sys) {
    ts.sys.writeOutputIsTTY = function() { return false; };
  }
  tsReady = true;
})();

function patchSys(projectCwd) {
  function resolvePath(p) {
    p = String(p);
    if (p.charAt(0) !== '/') p = projectCwd + '/' + p;
    return p;
  }
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
  var tsLibDir = __edgebox_cwd() + '/node_modules/typescript/lib';
  ts.sys.getExecutingFilePath = function() { return tsLibDir + '/typescript.js'; };
  ts.sys.write = function(s) { __edgebox_write_stdout(String(s)); };
  ts.sys.writeFile = function(p, data) { __edgebox_write_file(resolvePath(p), data); };
  // Do NOT call exit in daemon mode — return control to fetch handler
  ts.sys.exit = function() {};
}

export default {
  async fetch(request) {
    if (!tsReady) return new Response('TypeScript not loaded', { status: 500 });

    // Get project cwd from request body or project config
    var body = {};
    try { body = await request.json(); } catch(e) {}
    var projectCwd = body.cwd || '';

    if (!projectCwd) {
      // Fall back to project config file
      var configJson = __edgebox_read_file('/tmp/edgebox-project-config.json');
      if (configJson) {
        try { projectCwd = JSON.parse(configJson).cwd; } catch(e) {}
      }
    }

    if (!projectCwd) return new Response('No project cwd', { status: 400 });

    // Patch ts.sys for this project
    patchSys(projectCwd);

    // Capture diagnostics output
    var output = [];
    var origWrite = ts.sys.write;
    ts.sys.write = function(s) { output.push(String(s)); };

    // Run TSC
    var t0 = Date.now();
    var exitCode = 0;
    ts.sys.exit = function(code) { exitCode = code; };
    ts.sys.args = ['--noEmit', '-p', projectCwd + '/tsconfig.json'];
    try {
      ts.executeCommandLine(ts.sys, ts.noop, ts.sys.args);
    } catch(e) {
      output.push('error: ' + e.message + '\n');
      exitCode = 1;
    }
    var checkTime = Date.now() - t0;

    // Restore write
    ts.sys.write = origWrite;

    // Stream diagnostics as response body
    var diagnosticText = output.join('');
    return new Response(diagnosticText, {
      status: exitCode === 0 ? 200 : 422,
      headers: {
        'Content-Type': 'text/plain',
        'X-Check-Time': String(checkTime),
        'X-Exit-Code': String(exitCode),
        'X-Diagnostics': String(output.length),
      }
    });
  }
};
