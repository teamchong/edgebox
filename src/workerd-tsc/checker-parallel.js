import './bootstrap.js';
import ts from './typescript.js';

// Cache: reuse program if rootNames match, pass oldProgram for incremental
var cachedProgram = null;
var cachedRootKey = '';

export default {
  async fetch(request) {
    var body = await request.json();
    var start = Date.now();

    try {
      var rootKey = body.rootNames.join(',');
      var program;
      var parseTime;
      
      if (cachedProgram && cachedRootKey === rootKey) {
        // Exact match — reuse cached program (skip parse + check)
        program = cachedProgram;
        parseTime = 0;
      } else {
        // Create program — pass oldProgram for incremental lib.d.ts reuse
        program = ts.createProgram(
          body.rootNames,
          body.options,
          undefined, // host
          cachedProgram // oldProgram — TSC reuses unchanged SourceFiles
        );
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
