import './bootstrap.js';
import ts from './typescript.js';

// Each worker: pre-parse from config, then wait for shard assignment via Zig shared memory.
// No HTTP dispatch — pure Zig channel coordination.

var cachedProgram = null;
var cachedRootKey = '';

// Pre-parse at module load
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
    var host = ts.createCompilerHost(parsed.options);
    var libDir = config.cwd + '/node_modules/typescript/lib';
    // Try default lib resolution
    cachedProgram = ts.createProgram(parsed.fileNames, parsed.options);
    cachedRootKey = parsed.fileNames.join(',');
  } catch(e) {}
})();

export default {
  async fetch(request) {
    // HTTP fallback — also supports direct shard assignment via POST
    var body = await request.json().catch(function() { return {}; });
    var start = Date.now();
    try {
      var program = cachedProgram;
      if (!program) {
        return new Response(JSON.stringify({error: 'no cached program'}));
      }

      var diagnostics = [];
      var files = program.getSourceFiles();
      var wid = body.workerId || 0;
      var wcnt = body.workerCount || 1;

      for (var i = 0; i < files.length; i++) {
        if (i % wcnt !== wid) continue;
        var diags = program.getSemanticDiagnostics(files[i]);
        for (var k = 0; k < diags.length; k++) {
          var d = diags[k];
          if (d.file) {
            diagnostics.push({
              file: d.file.fileName,
              start: d.start || 0,
              code: d.code,
              message: ts.flattenDiagnosticMessageText(d.messageText, '\n'),
            });
          }
        }
      }

      return new Response(JSON.stringify({
        workerId: wid,
        diagnostics: diagnostics.length,
        filesChecked: Math.ceil(files.length / wcnt),
        checkTime: Date.now() - start,
        cached: true,
      }));
    } catch(e) {
      return new Response(JSON.stringify({error: e.message}));
    }
  }
};
