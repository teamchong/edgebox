import './bootstrap.js';
import ts from './typescript.js';

var cachedProgram = null;
var cachedRootKey = '';

// At module load: read pre-written config, parse immediately
(function earlyParse() {
  try {
    // Read config written by launch script (before workerd started)
    var configResp = JSON.parse(__edgebox_io_sync(JSON.stringify({
      op: 'readFile', path: '/tmp/edgebox-project-config.json'
    })));
    if (!configResp.ok) return;
    var projectConfig = JSON.parse(configResp.data);
    var cwd = projectConfig.cwd;
    
    // Read tsconfig
    var tsconfigResp = JSON.parse(__edgebox_io_sync(JSON.stringify({
      op: 'readFile', path: cwd + '/tsconfig.json'
    })));
    if (!tsconfigResp.ok) return;
    
    var configFile = ts.readConfigFile(cwd + '/tsconfig.json', function(p) {
      var r = JSON.parse(__edgebox_io_sync(JSON.stringify({op:'readFile',path:p})));
      return r.ok ? r.data : undefined;
    });
    var parsedConfig = ts.parseJsonConfigFileContent(configFile.config, ts.sys, cwd);
    
    cachedProgram = ts.createProgram(parsedConfig.fileNames, parsedConfig.options);
    cachedRootKey = parsedConfig.fileNames.join(',');
  } catch(e) {
    // Config not available — parse on first request
  }
})();

export default {
  async fetch(request) {
    var body = await request.json();
    var start = Date.now();

    try {
      var rootKey = body.rootNames.join(',');
      var program;
      var parseTime;
      
      if (cachedProgram && cachedRootKey === rootKey) {
        program = cachedProgram;
        parseTime = 0;
      } else {
        program = ts.createProgram(body.rootNames, body.options, undefined, cachedProgram);
        cachedProgram = program;
        cachedRootKey = rootKey;
        parseTime = Date.now() - start;
      }

      var assigned = {};
      for (var i = 0; i < body.files.length; i++) assigned[body.files[i]] = true;

      var checkStart = Date.now();
      var diagnostics = [];
      var sourceFiles = program.getSourceFiles();

      for (var j = 0; j < sourceFiles.length; j++) {
        if (!assigned[sourceFiles[j].fileName]) continue;
        var fileDiags = program.getSemanticDiagnostics(sourceFiles[j]);
        for (var k = 0; k < fileDiags.length; k++) {
          var d = fileDiags[k];
          diagnostics.push({
            file: d.file ? d.file.fileName : '',
            start: d.start || 0,
            code: d.code,
            message: ts.flattenDiagnosticMessageText(d.messageText, '\n'),
          });
        }
      }

      return new Response(JSON.stringify({
        workerId: body.workerId,
        diagnostics: diagnostics,
        filesChecked: body.files.length,
        parseTime: parseTime,
        checkTime: Date.now() - checkStart,
        cached: parseTime === 0,
      }));
    } catch(e) {
      return new Response(JSON.stringify({error: e.message, workerId: body.workerId}));
    }
  }
};
