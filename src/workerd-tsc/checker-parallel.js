import './bootstrap.js';
import './typescript.js';
var ts = globalThis.ts || globalThis.module.exports;

var WORKER_ID = parseInt(globalThis.__EDGEBOX_WORKER_ID || '0', 10);

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
  ts.sys.exit = function() {};
}

export default {
  fetch() {
    // Block on Zig condvar — wakes when main dispatches work
    // This is the zero-copy channel: Zig shared memory, no HTTP
    var workInfo = __edgebox_wait_for_work(WORKER_ID);
    if (!workInfo) return new Response('no work');

    var parts = workInfo.split('|');
    var projectCwd = parts[0];
    var workerId = parseInt(parts[1], 10);
    var workerCount = parseInt(parts[2], 10);

    if (!ts || !ts.createProgram) {
      __edgebox_submit_result(workerId, '');
      return new Response('no tsc');
    }

    patchSys(projectCwd);

    var configFile = ts.readConfigFile(projectCwd + '/tsconfig.json', ts.sys.readFile);
    if (configFile.error) { __edgebox_submit_result(workerId, ''); return new Response('config error'); }
    var parsed = ts.parseJsonConfigFileContent(configFile.config, ts.sys, projectCwd);
    var program = ts.createProgram(parsed.fileNames, parsed.options);

    var files = program.getSourceFiles();
    var output = [];

    if (workerId === 0) {
      var globalDiags = ts.getPreEmitDiagnostics(program).filter(function(d) { return !d.file; });
      for (var g = 0; g < globalDiags.length; g++) {
        output.push(ts.flattenDiagnosticMessageText(globalDiags[g].messageText, '\n'));
      }
    }

    for (var i = 0; i < files.length; i++) {
      if (i % workerCount !== workerId) continue;
      var diags = program.getSemanticDiagnostics(files[i]);
      for (var k = 0; k < diags.length; k++) {
        var d = diags[k];
        if (d.file) {
          var pos = d.file.getLineAndCharacterOfPosition(d.start || 0);
          output.push(d.file.fileName + '(' + (pos.line+1) + ',' + (pos.character+1) + '): error TS' + d.code + ': ' + ts.flattenDiagnosticMessageText(d.messageText, ' '));
        }
      }
    }

    __edgebox_submit_result(workerId, output.join('\n'));
    return new Response('done');
  }
};
