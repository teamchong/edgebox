import './bootstrap.js';
// TypeScript loaded as CommonJS — sets globalThis.ts
import './typescript.js';
var ts = globalThis.ts;

// Pre-check at module init — zero JSON in hot path
(function() {
  try {
    // IO still uses __edgebox_io_sync for file reads (needs JSON for path)
    // TODO: add __edgebox_read_file(path) direct typed method
    var resp = JSON.parse(__edgebox_io_sync('{"op":"readFile","path":"/tmp/edgebox-project-config.json"}'));
    if (!resp.ok) return;
    var config = JSON.parse(resp.data);
    var configFile = ts.readConfigFile(config.cwd + '/tsconfig.json', ts.sys.readFile);
    var parsed = ts.parseJsonConfigFileContent(configFile.config, ts.sys, config.cwd);
    var program = ts.createProgram(parsed.fileNames, parsed.options);

    var t0 = Date.now();
    var files = program.getSourceFiles();
    var diagCount = 0;

    // Check all files — write diagnostics directly to stdout (zero copy)
    for (var i = 0; i < files.length; i++) {
      var diags = program.getSemanticDiagnostics(files[i]);
      diagCount += diags.length;
      for (var k = 0; k < diags.length; k++) {
        var d = diags[k];
        if (d.file) {
          var pos = d.file.getLineAndCharacterOfPosition(d.start || 0);
          // Direct write — kj::String → Zig posix.write(1, ...) — ZERO JSON
          __edgebox_write_stdout(d.file.fileName + '(' + (pos.line+1) + ',' + (pos.character+1) + '): error TS' + d.code + ': ' + ts.flattenDiagnosticMessageText(d.messageText, ' ') + '\n');
        }
      }
    }
    var checkTime = Date.now() - t0;

    // Register types AFTER check (read-only, zero copy)
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

    // Status to stderr — direct write, zero JSON
    __edgebox_write_stderr('[checker] ' + diagCount + ' diagnostics, ' + files.length + ' files, ' + checkTime + 'ms\n');
  } catch(e) {
    __edgebox_write_stderr('[checker] error: ' + e.message + '\n');
  }
})();

export default {
  fetch() { return new Response('edgebox-checker'); }
};
