import './bootstrap.js';
import ts from './typescript.js';

export default {
  async fetch(request) {
    var body = await request.json();
    var start = Date.now();

    try {
      // Create own program (parse from Zig cache — files already warm from main)
      var program = ts.createProgram(body.rootNames, body.options);
      var parseTime = Date.now() - start;

      // Build assigned file set
      var assigned = {};
      for (var i = 0; i < body.files.length; i++) assigned[body.files[i]] = true;

      // Check only assigned files
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
      }));
    } catch(e) {
      return new Response(JSON.stringify({error: e.message, workerId: body.workerId}));
    }
  }
};
