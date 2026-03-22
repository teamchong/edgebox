import './bootstrap.js';
import ts from './typescript.js';

export default {
  async fetch(request, env) {
    var body = await request.json().catch(function() { return {}; });
    var projectDir = body.cwd || '/tmp/ts-test-project';
    var start = Date.now();

    try {
      if (!ts.sys) return new Response(JSON.stringify({error: 'ts.sys null'}));

      var configPath = projectDir + '/tsconfig.json';
      if (!ts.sys.fileExists(configPath)) {
        return new Response(JSON.stringify({error: 'No tsconfig.json'}));
      }
      var configFile = ts.readConfigFile(configPath, ts.sys.readFile);
      var parsedConfig = ts.parseJsonConfigFileContent(configFile.config, ts.sys, projectDir);

      var parseStart = Date.now();
      var program = ts.createProgram(parsedConfig.fileNames, parsedConfig.options);
      var sourceFiles = program.getSourceFiles();
      var parseTime = Date.now() - parseStart;

      // Collect available checkers
      var checkers = [];
      if (env.CHECKER_0) checkers.push(env.CHECKER_0);
      if (env.CHECKER_1) checkers.push(env.CHECKER_1);
      if (env.CHECKER_2) checkers.push(env.CHECKER_2);
      if (env.CHECKER_3) checkers.push(env.CHECKER_3);
      var workerCount = checkers.length;

      // Shard files
      var shards = [];
      for (var w = 0; w < workerCount; w++) {
        var files = [];
        for (var i = w; i < sourceFiles.length; i += workerCount) {
          files.push(sourceFiles[i].fileName);
        }
        shards.push(files);
      }

      // Dispatch in parallel
      var checkStart = Date.now();
      var promises = checkers.map(function(checker, idx) {
        return checker.fetch(new Request('http://check/', {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: JSON.stringify({
            workerId: idx,
            files: shards[idx],
            rootNames: parsedConfig.fileNames,
            options: parsedConfig.options,
            cwd: projectDir,
          })
        })).then(function(r) { return r.json(); });
      });

      var results = await Promise.all(promises);
      var checkTime = Date.now() - checkStart;

      var allDiags = [];
      var workerTimes = [];
      for (var r = 0; r < results.length; r++) {
        if (results[r].diagnostics) allDiags = allDiags.concat(results[r].diagnostics);
        workerTimes.push({parse: results[r].parseTime, check: results[r].checkTime, files: results[r].filesChecked});
      }

      // Dedup
      var seen = {};
      var unique = [];
      for (var d = 0; d < allDiags.length; d++) {
        var key = allDiags[d].file + ':' + allDiags[d].start + ':' + allDiags[d].code;
        if (!seen[key]) { seen[key] = true; unique.push(allDiags[d]); }
      }

      return new Response(JSON.stringify({
        mode: 'parallel',
        workers: workerCount,
        totalFiles: sourceFiles.length,
        diagnostics: unique.length,
        parseTime: parseTime,
        checkTime: checkTime,
        totalTime: Date.now() - start,
        workerTimes: workerTimes,
      }));
    } catch(e) {
      return new Response(JSON.stringify({error: e.message}));
    }
  }
};
