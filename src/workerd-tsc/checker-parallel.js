import './bootstrap.js';
import ts from './typescript.js';

// Cache: reuse program if rootNames + options match
var cachedProgram = null;
var cachedRootKey = '';

export default {
  async fetch(request) {
    var body = await request.json();
    var start = Date.now();

    try {
      // Cache key: rootNames hash
      var rootKey = body.rootNames.join(',');
      var program;
      
      if (cachedProgram && cachedRootKey === rootKey) {
        // Reuse cached program — skip parse entirely!
        program = cachedProgram;
        var parseTime = 0;
      } else {
        // First run: create program (parse from Zig cache)
        program = ts.createProgram(body.rootNames, body.options);
        cachedProgram = program;
        cachedRootKey = rootKey;
        var parseTime = Date.now() - start;
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
        cached: cachedRootKey === rootKey && parseTime === 0,
      }));
    } catch(e) {
      return new Response(JSON.stringify({error: e.message, workerId: body.workerId}));
    }
  }
};
