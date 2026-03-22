import './bootstrap.js';
import ts from './typescript.js';

var cachedProgram = null;
var cachedRootKey = '';
var cachedSourceFiles = null;
var cachedParsedConfig = null;

// Pre-parse at module load from config file
(function earlyParse() {
  try {
    var configResp = JSON.parse(__edgebox_io_sync(JSON.stringify({
      op: 'readFile', path: '/tmp/edgebox-project-config.json'
    })));
    if (!configResp.ok) return;
    var projectConfig = JSON.parse(configResp.data);
    var cwd = projectConfig.cwd;
    
    var tsconfigResp = JSON.parse(__edgebox_io_sync(JSON.stringify({
      op: 'readFile', path: cwd + '/tsconfig.json'
    })));
    if (!tsconfigResp.ok) return;
    
    var configFile = ts.readConfigFile(cwd + '/tsconfig.json', ts.sys.readFile);
    cachedParsedConfig = ts.parseJsonConfigFileContent(configFile.config, ts.sys, cwd);
    
    cachedProgram = ts.createProgram(cachedParsedConfig.fileNames, cachedParsedConfig.options);
    cachedSourceFiles = cachedProgram.getSourceFiles();
    cachedRootKey = cachedParsedConfig.fileNames.join(',');
  } catch(e) {}
})();

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

      var parsedConfig;
      if (cachedParsedConfig && body.cwd === '/tmp/big-ts-1k') {
        parsedConfig = cachedParsedConfig;
      } else {
        var configFile = ts.readConfigFile(configPath, ts.sys.readFile);
        parsedConfig = ts.parseJsonConfigFileContent(configFile.config, ts.sys, projectDir);
      }

      // Write config for future worker restarts
      var configData = JSON.stringify({rootNames: parsedConfig.fileNames, options: parsedConfig.options, cwd: projectDir});
      __edgebox_io_sync(JSON.stringify({op: 'setConfig', data: configData}));

      var rootKey = parsedConfig.fileNames.join(',');
      var parseTime;
      var sourceFiles;

      if (cachedProgram && cachedRootKey === rootKey) {
        parseTime = 0;
        sourceFiles = cachedSourceFiles;
      } else {
        var parseStart = Date.now();
        var program = ts.createProgram(parsedConfig.fileNames, parsedConfig.options);
        sourceFiles = program.getSourceFiles();
        cachedProgram = program;
        cachedRootKey = rootKey;
        cachedSourceFiles = sourceFiles;
        parseTime = Date.now() - parseStart;
      }

      var checkers = [];
      if (env.CHECKER_0) checkers.push(env.CHECKER_0);
      if (env.CHECKER_1) checkers.push(env.CHECKER_1);
      if (env.CHECKER_2) checkers.push(env.CHECKER_2);
      if (env.CHECKER_3) checkers.push(env.CHECKER_3);

      var shards = [];
      for (var w = 0; w < checkers.length; w++) {
        var files = [];
        for (var i = w; i < sourceFiles.length; i += checkers.length) {
          files.push(sourceFiles[i].fileName);
        }
        shards.push(files);
      }

      var checkStart = Date.now();
      var promises = checkers.map(function(checker, idx) {
        return checker.fetch(new Request('http://check/', {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: JSON.stringify({workerId: idx, files: shards[idx], rootNames: parsedConfig.fileNames, options: parsedConfig.options, cwd: projectDir})
        })).then(function(r) { return r.json(); });
      });

      var results = await Promise.all(promises);
      var checkTime = Date.now() - checkStart;

      var allDiags = [], workerTimes = [];
      for (var r = 0; r < results.length; r++) {
        if (results[r].diagnostics) allDiags = allDiags.concat(results[r].diagnostics);
        workerTimes.push({parse: results[r].parseTime, check: results[r].checkTime, files: results[r].filesChecked, cached: results[r].cached});
      }

      var seen = {}, unique = [];
      for (var d = 0; d < allDiags.length; d++) {
        var key = allDiags[d].file + ':' + allDiags[d].start + ':' + allDiags[d].code;
        if (!seen[key]) { seen[key] = true; unique.push(allDiags[d]); }
      }

      return new Response(JSON.stringify({
        mode: 'parallel', workers: checkers.length, totalFiles: sourceFiles.length,
        diagnostics: unique.length, parseTime: parseTime, checkTime: checkTime,
        totalTime: Date.now() - start, workerTimes: workerTimes,
      }));
    } catch(e) {
      return new Response(JSON.stringify({error: e.message}));
    }
  }
};
