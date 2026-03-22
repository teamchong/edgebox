// main-worker.js — EdgeBox Parallel TSC Orchestrator
//
// Architecture:
//   1. Load TypeScript compiler
//   2. Run createProgram (parse + bind all files) — SERIAL
//   3. Extract file list
//   4. Dispatch file shards to checker workers via RPC — PARALLEL
//   5. Collect diagnostics from all workers
//   6. Merge + dedup + output
//
// All IO via __edgebox_io_sync (Zig mmap cache — zero-copy after first read).
import { loadTsc } from './tsc-loader.js';

var WORKER_COUNT = 4;
var checkers = null;

export default {
  async fetch(request, env) {
    // Parse request: { args: string[], cwd: string }
    var body = await request.json().catch(function() { return {}; });
    var args = body.args || ['--noEmit', '-p', 'tsconfig.json'];
    var cwd = body.cwd || process.cwd();

    // Save checker service bindings
    checkers = [env.CHECKER_0, env.CHECKER_1, env.CHECKER_2, env.CHECKER_3];

    var startTime = Date.now();

    // 1. Load TSC
    var ts = loadTsc();

    // 2. Parse command line
    var commandLine = ts.parseCommandLine(args);
    var configPath = ts.findConfigFile(cwd, ts.sys.fileExists, 'tsconfig.json');
    var parsedConfig;

    if (configPath) {
      var configFile = ts.readConfigFile(configPath, ts.sys.readFile);
      parsedConfig = ts.parseJsonConfigFileContent(
        configFile.config,
        ts.sys,
        require('path').dirname(configPath),
        commandLine.options
      );
    } else {
      parsedConfig = {
        options: commandLine.options,
        fileNames: commandLine.fileNames,
        errors: commandLine.errors
      };
    }

    // 3. Create program (parse + bind all files) — this is the expensive serial part
    var parseStart = Date.now();
    var program = ts.createProgram({
      rootNames: parsedConfig.fileNames,
      options: parsedConfig.options,
      host: ts.createCompilerHost(parsedConfig.options)
    });
    var parseTime = Date.now() - parseStart;

    // 4. Get file list for sharding
    var sourceFiles = program.getSourceFiles();
    var fileNames = [];
    for (var i = 0; i < sourceFiles.length; i++) {
      fileNames.push(sourceFiles[i].fileName);
    }

    // 5. Dispatch shards to checker workers via RPC (parallel)
    var checkStart = Date.now();
    var shardPromises = [];

    for (var w = 0; w < WORKER_COUNT; w++) {
      var workerFiles = [];
      for (var j = w; j < fileNames.length; j += WORKER_COUNT) {
        workerFiles.push(fileNames[j]);
      }

      // RPC call to checker worker
      shardPromises.push(
        checkers[w].fetch(new Request('http://checker/', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({
            workerId: w,
            workerCount: WORKER_COUNT,
            files: workerFiles,
            options: parsedConfig.options,
            rootNames: parsedConfig.fileNames,
            cwd: cwd
          })
        })).then(function(resp) { return resp.json(); })
      );
    }

    // 6. Collect all diagnostics
    var results = await Promise.all(shardPromises);
    var checkTime = Date.now() - checkStart;

    // 7. Merge diagnostics
    var allDiagnostics = [];
    for (var r = 0; r < results.length; r++) {
      if (results[r].diagnostics) {
        for (var d = 0; d < results[r].diagnostics.length; d++) {
          allDiagnostics.push(results[r].diagnostics[d]);
        }
      }
    }

    // 8. Dedup by file+start+code
    var seen = {};
    var uniqueDiags = [];
    for (var k = 0; k < allDiagnostics.length; k++) {
      var diag = allDiagnostics[k];
      var key = (diag.file || '') + ':' + (diag.start || 0) + ':' + (diag.code || 0);
      if (!seen[key]) {
        seen[key] = true;
        uniqueDiags.push(diag);
      }
    }

    // 9. Format output (same as TSC)
    var output = '';
    for (var m = 0; m < uniqueDiags.length; m++) {
      var ud = uniqueDiags[m];
      output += ud.text + '\n';
    }

    var totalTime = Date.now() - startTime;

    // Write diagnostics to stderr (like TSC does)
    if (output) {
      var resp = JSON.parse(__edgebox_io_sync(JSON.stringify({
        op: 'writeErr', data: output
      })));
    }

    // Summary to stderr
    var summary = '[parallel-tsc] ' + fileNames.length + ' files, ' +
      WORKER_COUNT + ' workers, ' + uniqueDiags.length + ' errors\n' +
      '[parallel-tsc] parse=' + parseTime + 'ms check=' + checkTime + 'ms total=' + totalTime + 'ms\n';
    __edgebox_io_sync(JSON.stringify({ op: 'writeErr', data: summary }));

    // Exit with appropriate code
    var exitCode = uniqueDiags.length > 0 ? 2 : 0;
    __edgebox_io_sync(JSON.stringify({ op: 'exit', code: exitCode }));

    return new Response(JSON.stringify({
      diagnostics: uniqueDiags.length,
      parseTime: parseTime,
      checkTime: checkTime,
      totalTime: totalTime,
      files: fileNames.length
    }), { headers: { 'Content-Type': 'application/json' } });
  }
};
