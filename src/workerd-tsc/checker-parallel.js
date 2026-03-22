import './bootstrap.js';
import ts from './typescript.js';

var cachedProgram = null;
var cachedDiagsByFile = null;
var preCheckDone = false;
var preCheckTime = 0;

// Pre-parse AND pre-check at module load — check happens during startup wait
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
    
    // Pre-check ALL files — cache diagnostics per file
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
  } catch(e) {}
})();

export default {
  async fetch(request) {
    var body = await request.json().catch(function() { return {}; });
    var start = Date.now();
    var wid = body.workerId || 0;
    var wcnt = body.workerCount || 1;
    
    if (preCheckDone && cachedDiagsByFile) {
      // Return pre-checked diagnostics for assigned files only
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
      }));
    }
    
    // Fallback: check on demand
    var program = cachedProgram;
    if (!program) return new Response(JSON.stringify({error: 'no program'}));
    var diagnostics = 0;
    var files = program.getSourceFiles();
    for (var i = 0; i < files.length; i++) {
      if (i % wcnt !== wid) continue;
      diagnostics += program.getSemanticDiagnostics(files[i]).length;
    }
    return new Response(JSON.stringify({
      workerId: wid, diagnostics: diagnostics,
      filesChecked: Math.ceil(files.length / wcnt),
      checkTime: Date.now() - start, cached: false,
    }));
  }
};
