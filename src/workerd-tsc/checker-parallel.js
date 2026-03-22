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
    
    // Check FIRST (no type registration yet — pure vanilla TSC)
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
    
    // Register types AFTER checking (read-only extraction, no side effects)
    var checker = cachedProgram.getTypeChecker();
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
              if (prop.escapedName && pt && pt.id) {
                __edgebox_register_member(type.id, prop.escapedName, pt.id, prop.flags || 0);
              }
            }
          }
        } catch(e) {}
        ts.forEachChild(node, visit);
      });
    }
  } catch(e) {}
})();

export default {
  async fetch(request) {
    var body = await request.json().catch(function() { return {}; });
    var wid = body.workerId || 0;
    var wcnt = body.workerCount || 1;
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
        checkTime: 0, preCheckTime: preCheckTime, cached: true,
        zigTypes: stats.types, zigMembers: stats.members, zigStrings: stats.strings,
      }));
    }
    return new Response(JSON.stringify({error: 'not ready'}));
  }
};
