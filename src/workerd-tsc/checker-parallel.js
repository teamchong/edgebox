import './bootstrap.js';
import ts from './typescript.js';

var cachedProgram = null;
var cachedRootKey = '';
var cachedDiagnostics = null; // Pre-computed diagnostics per file

// At module load: parse AND check the full program
(function earlyParseAndCheck() {
  try {
    var configResp = JSON.parse(__edgebox_io_sync(JSON.stringify({
      op: 'readFile', path: '/tmp/edgebox-project-config.json'
    })));
    if (!configResp.ok) return;
    var projectConfig = JSON.parse(configResp.data);
    var cwd = projectConfig.cwd;
    
    var configFile = ts.readConfigFile(cwd + '/tsconfig.json', function(p) {
      var r = JSON.parse(__edgebox_io_sync(JSON.stringify({op:'readFile',path:p})));
      return r.ok ? r.data : undefined;
    });
    var parsedConfig = ts.parseJsonConfigFileContent(configFile.config, ts.sys, cwd);
    
    cachedProgram = ts.createProgram(parsedConfig.fileNames, parsedConfig.options);
    cachedRootKey = parsedConfig.fileNames.join(',');
    
    // Pre-check ALL files — cache diagnostics per file
    cachedDiagnostics = {};
    var sourceFiles = cachedProgram.getSourceFiles();
    for (var i = 0; i < sourceFiles.length; i++) {
      var sf = sourceFiles[i];
      var fileDiags = cachedProgram.getSemanticDiagnostics(sf);
      if (fileDiags.length > 0) {
        cachedDiagnostics[sf.fileName] = fileDiags.map(function(d) {
          return {
            file: d.file ? d.file.fileName : '',
            start: d.start || 0,
            code: d.code,
            message: ts.flattenDiagnosticMessageText(d.messageText, '\n'),
          };
        });
      }
    }
  } catch(e) {}
})();

export default {
  async fetch(request) {
    var body = await request.json();
    var start = Date.now();

    try {
      // Return pre-computed diagnostics for assigned files only
      var diagnostics = [];
      if (cachedDiagnostics) {
        for (var i = 0; i < body.files.length; i++) {
          var fileDiags = cachedDiagnostics[body.files[i]];
          if (fileDiags) {
            for (var j = 0; j < fileDiags.length; j++) {
              diagnostics.push(fileDiags[j]);
            }
          }
        }
      } else {
        // Fallback: check on demand
        if (!cachedProgram) {
          cachedProgram = ts.createProgram(body.rootNames, body.options, undefined, cachedProgram);
          cachedRootKey = body.rootNames.join(',');
        }
        var assigned = {};
        for (var i = 0; i < body.files.length; i++) assigned[body.files[i]] = true;
        var sourceFiles = cachedProgram.getSourceFiles();
        for (var j = 0; j < sourceFiles.length; j++) {
          if (!assigned[sourceFiles[j].fileName]) continue;
          var fileDiags = cachedProgram.getSemanticDiagnostics(sourceFiles[j]);
          for (var k = 0; k < fileDiags.length; k++) {
            var d = fileDiags[k];
            diagnostics.push({file: d.file ? d.file.fileName : '', start: d.start || 0, code: d.code, message: ts.flattenDiagnosticMessageText(d.messageText, '\n')});
          }
        }
      }

      return new Response(JSON.stringify({
        workerId: body.workerId,
        diagnostics: diagnostics,
        filesChecked: body.files.length,
        parseTime: 0,
        checkTime: Date.now() - start,
        cached: !!cachedDiagnostics,
      }));
    } catch(e) {
      return new Response(JSON.stringify({error: e.message, workerId: body.workerId}));
    }
  }
};
