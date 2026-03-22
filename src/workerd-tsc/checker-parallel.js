import './bootstrap.js';
// TypeScript loaded as CommonJS — writes to module.exports
import './typescript.js';
var ts = globalThis.ts || globalThis.module.exports;

// Pre-check at module init — zero JSON in hot path
(function() {
  try {
    if (!ts || !ts.createProgram) {
      __edgebox_write_stderr('[checker] TypeScript not loaded\n');
      return;
    }

    // Set up ts.sys to use Zig IO
    if (ts.sys) {
      ts.sys.readFile = function(p) { return __edgebox_read_file(String(p)) || undefined; };
      ts.sys.fileExists = function(p) { return __edgebox_file_exists(String(p)) === 1; };
      ts.sys.directoryExists = function(p) { return __edgebox_dir_exists(String(p)) === 1; };
      ts.sys.getDirectories = function(p) {
        var entries = JSON.parse(__edgebox_readdir(String(p)));
        return entries.filter(function(e) { return __edgebox_dir_exists(String(p) + '/' + e) === 1; });
      };
      ts.sys.readDirectory = function(p) {
        return JSON.parse(__edgebox_readdir(String(p)));
      };
      ts.sys.realpath = function(p) { return __edgebox_realpath(String(p)); };
      ts.sys.getCurrentDirectory = function() { return __edgebox_cwd(); };
      ts.sys.getExecutingFilePath = function() { return __filename; };
      ts.sys.write = function(s) { __edgebox_write_stdout(String(s)); };
      ts.sys.writeOutputIsTTY = function() { return false; };
      ts.sys.exit = function(code) { __edgebox_exit(code || 0); };
    }

    // Read project config
    var configJson = __edgebox_read_file('/tmp/edgebox-project-config.json');
    if (!configJson) return;
    var config = JSON.parse(configJson);
    var configFile = ts.readConfigFile(config.cwd + '/tsconfig.json', ts.sys.readFile);
    var parsed = ts.parseJsonConfigFileContent(configFile.config, ts.sys, config.cwd);
    var program = ts.createProgram(parsed.fileNames, parsed.options);

    var t0 = Date.now();
    var files = program.getSourceFiles();
    var diagCount = 0;

    // Check all files — write diagnostics directly via Zig
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

    // Register types AFTER check (zero copy)
    var checker = program.getTypeChecker();
    var seenTypes = {};
    for (var fi = 0; fi < files.length; fi++) {
      ts.forEachChild(files[fi], function visit(node) {
        try {
          var type = checker.getTypeAtLocation(node);
          if (type && type.id && !seenTypes[type.id]) {
            seenTypes[type.id] = true;
            __edgebox_register_type(type.id, type.flags || 0);
            var props = checker.getPropertiesOfType(type);
            for (var p = 0; p < props.length && p < 50; p++) {
              var prop = props[p];
              var pt = checker.getTypeOfSymbol(prop);
              if (prop.escapedName && pt && pt.id)
                __edgebox_register_member(type.id, prop.escapedName, pt.id, prop.flags || 0);
            }
          }
        } catch(e) {}
        ts.forEachChild(node, visit);
      });
    }

    __edgebox_write_stderr('[checker] ' + diagCount + ' diagnostics, ' + files.length + ' files, ' + checkTime + 'ms\n');
  } catch(e) {
    __edgebox_write_stderr('[checker] error: ' + e.message + '\n' + (e.stack || '') + '\n');
  }
})();

export default {
  fetch() { return new Response('edgebox-checker'); }
};
