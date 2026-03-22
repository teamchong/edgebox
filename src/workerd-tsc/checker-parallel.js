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
        // Use TSC's built-in matchFiles with our fs callbacks
        return ts.matchFiles(rootDir, extensions, excludes, includes, true, resolvePath(rootDir), depth, function(p) {
          var rp = resolvePath(p);
          var entries = JSON.parse(__edgebox_readdir(rp));
          var files = [], dirs = [];
          for (var i = 0; i < entries.length; i++) {
            var full = rp + '/' + entries[i];
            if (__edgebox_dir_exists(full) === 1) dirs.push(entries[i]);
            else files.push(entries[i]);
          }
          return { files: files, directories: dirs };
        }, function(p) { return __edgebox_realpath(resolvePath(p)); });
      };
      ts.sys.realpath = function(p) { return __edgebox_realpath(resolvePath(p)); };
      ts.sys.getCurrentDirectory = function() { return projectCwd; };
      ts.sys.getExecutingFilePath = function() { return __filename; };
      ts.sys.write = function(s) { __edgebox_write_stdout(String(s)); };
      ts.sys.writeOutputIsTTY = function() { return false; };
      ts.sys.exit = function(code) { __edgebox_exit(code || 0); };
    }

    // Parse tsconfig and create program
    var configFile = ts.readConfigFile(projectCwd + '/tsconfig.json', ts.sys.readFile);
    var parsed = ts.parseJsonConfigFileContent(configFile.config, ts.sys, projectCwd);
    console.log('[checker] ' + parsed.fileNames.length + ' root files');
    var program = ts.createProgram(parsed.fileNames, parsed.options);

    var t0 = Date.now();
    var files = program.getSourceFiles();
    var diagCount = 0;

    for (var i = 0; i < files.length; i++) {
      var diags = program.getSemanticDiagnostics(files[i]);
      diagCount += diags.length;
      for (var k = 0; k < diags.length; k++) {
        var d = diags[k];
        if (d.file) {
          var pos = d.file.getLineAndCharacterOfPosition(d.start || 0);
          __edgebox_write_stdout(d.file.fileName + '(' + (pos.line+1) + ',' + (pos.character+1) + '): error TS' + d.code + ': ' + ts.flattenDiagnosticMessageText(d.messageText, ' ') + '\n');
        }
      }
    }
    var checkTime = Date.now() - t0;
    console.log('[checker] ' + diagCount + ' diagnostics, ' + files.length + ' files, ' + checkTime + 'ms');
  } catch(e) {
    console.log('[checker] error: ' + e.message);
    console.log(e.stack || '');
  }
})();

export default {
  fetch() { return new Response('edgebox-checker'); }
};
