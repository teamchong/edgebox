// TSC Recipe — loaded once by V8 pool, called per request via __edgebox_check
//
// The Zig structural check is injected directly into TSC's internal isTypeRelatedTo
// via the recipe transform in v8_pool.zig (applied before snapshot creation).
// This recipe only handles: ts.sys setup, program creation, sharded diagnostics.
var ts = globalThis.ts || globalThis.module.exports;

globalThis.__edgebox_check = function(cwd, workerId, workerCount) {
  try {
  if (!ts || !ts.createProgram) return 'no tsc: ' + typeof ts;

  // Create ts.sys if missing (snapshot restore doesn't init it)
  if (!ts.sys && ts.setSys) {
    ts.setSys({
      args: [], newLine: '\n', useCaseSensitiveFileNames: true,
      write: function(s) { __edgebox_write_stdout(String(s)); },
      writeOutputIsTTY: function() { return false; },
      readFile: function() { return undefined; },
      writeFile: function() {},
      fileExists: function() { return false; },
      directoryExists: function() { return false; },
      createDirectory: function() {},
      getExecutingFilePath: function() { return '/'; },
      getCurrentDirectory: function() { return '/'; },
      getDirectories: function() { return []; },
      readDirectory: function() { return []; },
      exit: function() {},
      realpath: function(p) { return p; },
      getEnvironmentVariable: function() { return ''; },
    });
  }
  if (!ts.sys) return 'no sys (setSys failed)';

  var ebRoot = typeof __edgebox_root === 'function' ? __edgebox_root() : __edgebox_cwd();
  function rp(p) { p = String(p); return p.charAt(0) === '/' ? p : cwd + '/' + p; }

  // Wire ts.sys methods to Zig IO (zero-copy via C ABI)
  ts.sys.readFile = function(p) {
    var c = __edgebox_read_file(rp(p));
    if (!c) {
      var base = p.split('/').pop();
      if (base && base.indexOf('lib.') === 0 && base.endsWith('.d.ts'))
        c = __edgebox_read_file(ebRoot + '/node_modules/typescript/lib/' + base);
    }
    return c || undefined;
  };
  ts.sys.fileExists = function(p) {
    if (__edgebox_file_exists(rp(p)) === 1) return true;
    var base = p.split('/').pop();
    if (base && base.indexOf('lib.') === 0 && base.endsWith('.d.ts'))
      return __edgebox_file_exists(ebRoot + '/node_modules/typescript/lib/' + base) === 1;
    return false;
  };
  ts.sys.directoryExists = function(p) {
    return __edgebox_dir_exists(rp(p)) === 1 || (p.charAt(0) === '/' && __edgebox_dir_exists(p) === 1);
  };
  ts.sys.getCurrentDirectory = function() { return cwd; };
  ts.sys.realpath = function(p) { return __edgebox_realpath(rp(p)); };
  ts.sys.getExecutingFilePath = function() { return ebRoot + '/node_modules/typescript/lib/typescript.js'; };
  ts.sys.writeFile = function(p, data) { __edgebox_write_file(rp(p), data); };
  ts.sys.writeOutputIsTTY = function() { return false; };
  ts.sys.exit = function() {};
  ts.sys.useCaseSensitiveFileNames = true;
  globalThis.__filename = ebRoot + '/node_modules/typescript/lib/typescript.js';
  // readdir returns pre-typed: {"f":["file1"],"d":["dir1"]}
  ts.sys.getDirectories = function(p) {
    var rr = rp(p); var json = __edgebox_readdir(rr);
    if (!json || json.charAt(0) !== '{') return [];
    var typed = JSON.parse(json);
    return typed.d || [];
  };
  ts.sys.readDirectory = function(rootDir, ext, exc, inc, depth) {
    return ts.matchFiles(rootDir, ext, exc, inc, true, cwd, depth, function(p) {
      var rr = p || '.'; var json = __edgebox_readdir(rr);
      if (!json || json.charAt(0) !== '{') return { files: [], directories: [] };
      var typed = JSON.parse(json);
      return { files: typed.f || [], directories: typed.d || [] };
    }, function(p) { return __edgebox_realpath(p); });
  };

  // Create program (cached across requests on same worker)
  if (!globalThis.__pc) globalThis.__pc = {};
  if (!globalThis.__cc) globalThis.__cc = {};
  var t0 = Date.now();
  var parsed;
  if (globalThis.__cc[cwd]) {
    parsed = globalThis.__cc[cwd];
  } else {
    var cf = ts.readConfigFile(cwd + '/tsconfig.json', ts.sys.readFile);
    if (cf.error) return 'config error: ' + ts.flattenDiagnosticMessageText(cf.error.messageText, ' ');
    if (!cf.config) return 'config null';
    parsed = ts.parseJsonConfigFileContent(cf.config, ts.sys, cwd);
    globalThis.__cc[cwd] = parsed;
  }
  var ck = cwd + ':' + parsed.fileNames.length;
  var isWarm = !!globalThis.__pc[ck];
  var t1 = Date.now();
  var program;
  if (isWarm) {
    program = globalThis.__pc[ck];
  } else {
    program = ts.createProgram(parsed.fileNames, parsed.options);
    globalThis.__pc[ck] = program;
  }
  var t2 = Date.now();
  var files = program.getSourceFiles();
  var NL = String.fromCharCode(10);
  var output = [];
  var filesChecked = 0;

  // Filter to project source files (skip .d.ts — skipLibCheck makes them instant)
  var checkFiles = [];
  for (var fi = 0; fi < files.length; fi++)
    if (!files[fi].isDeclarationFile) checkFiles.push(files[fi]);

  if (isWarm) {
    // Hot: static sharding — TSC caches diagnostics per file in-memory
    for (var i = workerId; i < checkFiles.length; i += workerCount) {
      filesChecked++;
      var diags = program.getSemanticDiagnostics(checkFiles[i]);
      for (var k = 0; k < diags.length; k++) {
        var d = diags[k];
        if (d.file) {
          var pos = d.file.getLineAndCharacterOfPosition(d.start || 0);
          output.push(d.file.fileName + '(' + (pos.line+1) + ',' + (pos.character+1) + '): error TS' + d.code + ': ' + ts.flattenDiagnosticMessageText(d.messageText, ' '));
        }
      }
    }
  } else {
    // Cold: work-stealing — atomic counter balances load across workers
    while (true) {
      var idx = __edgebox_claim_file();
      if (idx >= checkFiles.length) break;
      filesChecked++;
      var diags = program.getSemanticDiagnostics(checkFiles[idx]);
      for (var k = 0; k < diags.length; k++) {
        var d = diags[k];
        if (d.file) {
          var pos = d.file.getLineAndCharacterOfPosition(d.start || 0);
          output.push(d.file.fileName + '(' + (pos.line+1) + ',' + (pos.character+1) + '): error TS' + d.code + ': ' + ts.flattenDiagnosticMessageText(d.messageText, ' '));
        }
      }
    }
  }

  // Worker 0 collects global diagnostics
  if (workerId === 0) {
    var gd = (program.getGlobalDiagnostics ? program.getGlobalDiagnostics() : [])
      .concat(program.getOptionsDiagnostics ? program.getOptionsDiagnostics() : [])
      .concat(program.getConfigFileParsingDiagnostics ? program.getConfigFileParsingDiagnostics() : []);
    for (var g = 0; g < gd.length; g++) {
      if (!gd[g].file)
        output.push('error TS' + gd[g].code + ': ' + ts.flattenDiagnosticMessageText(gd[g].messageText, ' '));
    }
  }

  var t3 = Date.now();
  __edgebox_write_stderr('[recipe] w' + workerId + '/' + workerCount + ' config:' + (t1-t0) + 'ms parse:' + (t2-t1) + 'ms check:' + (t3-t2) + 'ms total:' + (t3-t0) + 'ms checked:' + filesChecked + '/' + checkFiles.length + String.fromCharCode(10));
  return output.join(NL);
  } catch(e) { return '[recipe-error] w' + workerId + ': ' + (e && e.stack ? e.stack : String(e)); }
};
