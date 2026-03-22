import './bootstrap.js';
import ts from './typescript.js';

// Pre-check at module init — results submitted to Zig shared memory
(function() {
  try {
    var resp = JSON.parse(__edgebox_io_sync(JSON.stringify({op:'readFile',path:'/tmp/edgebox-project-config.json'})));
    if (!resp.ok) return;
    var config = JSON.parse(resp.data);
    var configFile = ts.readConfigFile(config.cwd + '/tsconfig.json', function(p) {
      var r = JSON.parse(__edgebox_io_sync(JSON.stringify({op:'readFile',path:p})));
      return r.ok ? r.data : undefined;
    });
    var parsed = ts.parseJsonConfigFileContent(configFile.config, ts.sys, config.cwd);
    var program = ts.createProgram(parsed.fileNames, parsed.options);
    
    var t0 = Date.now();
    var files = program.getSourceFiles();
    var diagCount = 0;
    var diagLines = [];
    
    for (var i = 0; i < files.length; i++) {
      var diags = program.getSemanticDiagnostics(files[i]);
      diagCount += diags.length;
      for (var k = 0; k < diags.length; k++) {
        var d = diags[k];
        if (d.file) {
          var pos = d.file.getLineAndCharacterOfPosition(d.start || 0);
          diagLines.push(d.file.fileName + '(' + (pos.line+1) + ',' + (pos.character+1) + '): error TS' + d.code + ': ' + ts.flattenDiagnosticMessageText(d.messageText, ' '));
        }
      }
    }
    var checkTime = Date.now() - t0;
    
    // Register types AFTER check (read-only)
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
    
    // Submit results to Zig shared memory — NO HTTP
    __edgebox_io_sync(JSON.stringify({
      op: 'submitResult',
      workerId: 0,
      data: JSON.stringify({
        diagnostics: diagCount,
        checkTime: checkTime,
        files: files.length,
        lines: diagLines,
      })
    }));
    
    // Write diagnostics to stderr (visible in terminal)
    __edgebox_io_sync(JSON.stringify({
      op: 'writeErr',
      data: '[checker] ' + diagCount + ' diagnostics, ' + files.length + ' files, ' + checkTime + 'ms\n'
    }));
  } catch(e) {
    __edgebox_io_sync(JSON.stringify({op:'writeErr', data:'[checker] error: ' + e.message + '\n'}));
  }
})();

// Dummy fetch handler (workerd requires it but we don't use HTTP)
export default {
  fetch() { return new Response('edgebox-checker'); }
};
