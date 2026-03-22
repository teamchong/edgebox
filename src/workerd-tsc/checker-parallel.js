import './bootstrap.js';
import ts from './typescript.js';

var cachedProgram = null;
var cachedRootKey = '';

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
    cachedRootKey = parsed.fileNames.join(',');
  } catch(e) {}
})();

export default {
  async fetch(request) {
    var body = await request.json();
    var startWall = Date.now();
    try {
      var program = cachedProgram;
      if (!program || cachedRootKey !== body.rootNames.join(',')) {
        program = ts.createProgram(body.rootNames, body.options, undefined, cachedProgram);
        cachedProgram = program;
        cachedRootKey = body.rootNames.join(',');
      }

      var diagnostics = [];
      var files = program.getSourceFiles();
      for (var i = 0; i < body.files.length; i++) {
        for (var j = 0; j < files.length; j++) {
          if (files[j].fileName === body.files[i]) {
            var diags = program.getSemanticDiagnostics(files[j]);
            for (var k = 0; k < diags.length; k++) {
              var d = diags[k];
              if (d.file && d.file.fileName === files[j].fileName) {
                diagnostics.push({file: d.file.fileName, start: d.start||0, code: d.code,
                  message: ts.flattenDiagnosticMessageText(d.messageText, '\n')});
              }
            }
            break;
          }
        }
      }

      return new Response(JSON.stringify({
        workerId: body.workerId, diagnostics: diagnostics,
        filesChecked: body.files.length, parseTime: 0,
        checkTime: Date.now() - startWall, cached: true,
        startMs: startWall, endMs: Date.now(),
      }));
    } catch(e) {
      return new Response(JSON.stringify({error: e.message, workerId: body.workerId}));
    }
  }
};
