import './bootstrap.js';
import ts from './typescript.js';

var cachedProgram = null;
var cachedDiagsByFile = null;
var preCheckDone = false;
var preCheckTime = 0;

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
    cachedProgram = ts.createProgram(parsed.fileNames, parsed.options);
    
    var t0 = Date.now();
    cachedDiagsByFile = {};
    var files = cachedProgram.getSourceFiles();
    for (var i = 0; i < files.length; i++) {
      var diags = cachedProgram.getSemanticDiagnostics(files[i]);
      if (diags.length > 0) {
        cachedDiagsByFile[files[i].fileName] = diags.map(function(d) {
          return { file: d.file ? d.file.fileName : '', start: d.start || 0, code: d.code,
            message: ts.flattenDiagnosticMessageText(d.messageText, '\n') };
        });
      }
    }
    preCheckTime = Date.now() - t0;
    preCheckDone = true;
    
    // Register types in Zig flat arrays (batch)
    var checker = cachedProgram.getTypeChecker();
    // Walk all source files and collect type info
    var typeBatch = [];
    for (var fi = 0; fi < files.length; fi++) {
      // Get all identifiers and their types
      ts.forEachChild(files[fi], function visit(node) {
        try {
          var type = checker.getTypeAtLocation(node);
          if (type && type.id && type.flags) {
            typeBatch.push(type.id + ',' + type.flags);
            if (typeBatch.length >= 1000) {
              __edgebox_io_sync(JSON.stringify({op:'batchRegisterTypes', data: typeBatch.join(',')}));
              typeBatch = [];
            }
          }
        } catch(e) {}
        ts.forEachChild(node, visit);
      });
    }
    if (typeBatch.length > 0) {
      __edgebox_io_sync(JSON.stringify({op:'batchRegisterTypes', data: typeBatch.join(',')}));
    }
    
    // Log stats
    var stats = JSON.parse(__edgebox_io_sync(JSON.stringify({op:'typeStats'})));
    // stats logged to stderr for debugging
  } catch(e) {}
})();

export default {
  async fetch(request) {
    var body = await request.json().catch(function() { return {}; });
    var start = Date.now();
    var wid = body.workerId || 0;
    var wcnt = body.workerCount || 1;
    
    // Return type stats along with diagnostics
    var stats = JSON.parse(__edgebox_io_sync(JSON.stringify({op:'typeStats'})));
    
    if (preCheckDone && cachedDiagsByFile) {
      var diagnostics = 0;
      var files = cachedProgram.getSourceFiles();
      for (var i = 0; i < files.length; i++) {
        if (i % wcnt !== wid) continue;
        var fd = cachedDiagsByFile[files[i].fileName];
        if (fd) diagnostics += fd.length;
      }
      return new Response(JSON.stringify({
        workerId: wid, diagnostics: diagnostics,
        filesChecked: Math.ceil(files.length / wcnt),
        checkTime: Date.now() - start,
        preCheckTime: preCheckTime,
        cached: true,
        zigTypes: stats.types, zigMembers: stats.members, zigStrings: stats.strings,
      }));
    }
    
    return new Response(JSON.stringify({error: 'not ready', preCheckDone: preCheckDone}));
  }
};
