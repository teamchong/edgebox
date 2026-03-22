// checker-worker.js — EdgeBox Parallel Type Checker
//
// Receives file shard from main worker via RPC.
// Creates its own TypeChecker on the full program, but only checks assigned files.
// Returns diagnostics for assigned files only.
//
// Key insight: each checker worker creates its own ts.createProgram.
// The parse phase reads from Zig's mmap cache (zero-copy, already warm from main).
// The check phase only runs on assigned files.
import { loadTsc } from './tsc-loader.js';

export default {
  async fetch(request) {
    var body = await request.json();
    var workerId = body.workerId;
    var workerCount = body.workerCount;
    var assignedFiles = body.files;
    var options = body.options;
    var rootNames = body.rootNames;

    var startTime = Date.now();

    // Load TSC (cached after first load)
    var ts = loadTsc();

    // Create program — parse phase reads from Zig mmap cache (warm, fast)
    var program = ts.createProgram({
      rootNames: rootNames,
      options: options,
      host: ts.createCompilerHost(options)
    });

    var parseTime = Date.now() - startTime;
    var checkStart = Date.now();

    // Build set of assigned files for fast lookup
    var assignedSet = {};
    for (var i = 0; i < assignedFiles.length; i++) {
      assignedSet[assignedFiles[i]] = true;
    }

    // Get diagnostics only for assigned files
    var diagnostics = [];
    var sourceFiles = program.getSourceFiles();

    for (var j = 0; j < sourceFiles.length; j++) {
      var sf = sourceFiles[j];
      if (!assignedSet[sf.fileName]) continue;

      // Get semantic diagnostics for this file
      var fileDiags = program.getSemanticDiagnostics(sf);
      for (var k = 0; k < fileDiags.length; k++) {
        var d = fileDiags[k];
        var formatted = ts.formatDiagnosticsWithColorAndContext([d], {
          getCanonicalFileName: function(f) { return f; },
          getCurrentDirectory: function() { return process.cwd(); },
          getNewLine: function() { return '\n'; }
        });
        diagnostics.push({
          file: d.file ? d.file.fileName : '',
          start: d.start || 0,
          length: d.length || 0,
          code: d.code,
          category: d.category,
          text: formatted.trim()
        });
      }

      // Also get syntactic diagnostics
      var syntaxDiags = program.getSyntacticDiagnostics(sf);
      for (var s = 0; s < syntaxDiags.length; s++) {
        var sd = syntaxDiags[s];
        var sformatted = ts.formatDiagnosticsWithColorAndContext([sd], {
          getCanonicalFileName: function(f) { return f; },
          getCurrentDirectory: function() { return process.cwd(); },
          getNewLine: function() { return '\n'; }
        });
        diagnostics.push({
          file: sd.file ? sd.file.fileName : '',
          start: sd.start || 0,
          length: sd.length || 0,
          code: sd.code,
          category: sd.category,
          text: sformatted.trim()
        });
      }
    }

    var checkTime = Date.now() - checkStart;

    return new Response(JSON.stringify({
      workerId: workerId,
      diagnostics: diagnostics,
      filesChecked: assignedFiles.length,
      parseTime: parseTime,
      checkTime: checkTime
    }), { headers: { 'Content-Type': 'application/json' } });
  }
};
