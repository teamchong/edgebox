// TSC Recipe — loaded once by V8 pool, called per request via __edgebox_check
var ts = globalThis.ts || globalThis.module.exports;

globalThis.__edgebox_check = function(cwd, workerId, workerCount) {
  if (!ts || !ts.createProgram) return 'no tsc';
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

  var ebRoot = __edgebox_cwd();
  function rp(p) { p = String(p); return p.charAt(0) === '/' ? p : cwd + '/' + p; }

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
  ts.sys.getDirectories = function(p) {
    var rr = rp(p); var entries = JSON.parse(__edgebox_readdir(rr));
    return entries.filter(function(e) { return __edgebox_dir_exists(rr + '/' + e) === 1; });
  };
  ts.sys.readDirectory = function(rootDir, ext, exc, inc, depth) {
    return ts.matchFiles(rootDir, ext, exc, inc, true, cwd, depth, function(p) {
      var rr = p || '.'; var json = __edgebox_readdir(rr);
      if (!json || json === '[]') return { files: [], directories: [] };
      var entries = JSON.parse(json); var files = [], dirs = [];
      for (var i = 0; i < entries.length; i++) {
        if (entries[i] === '.' || entries[i] === '..') continue;
        if (__edgebox_dir_exists(rr + '/' + entries[i]) === 1) dirs.push(entries[i]); else files.push(entries[i]);
      }
      return { files: files, directories: dirs };
    }, function(p) { return __edgebox_realpath(p); });
  };

  var cf = ts.readConfigFile(cwd + '/tsconfig.json', ts.sys.readFile);
  if (cf.error) return 'config error: ' + ts.flattenDiagnosticMessageText(cf.error.messageText, ' ');
  if (!cf.config) return 'config null';
  var parsed = ts.parseJsonConfigFileContent(cf.config, ts.sys, cwd);
  if (!globalThis.__pc) globalThis.__pc = {};
  var ck = cwd + ':' + parsed.fileNames.length;
  var program = globalThis.__pc[ck] || (globalThis.__pc[ck] = ts.createProgram(parsed.fileNames, parsed.options));
  var files = program.getSourceFiles();
  var NL = String.fromCharCode(10);
  var output = [];

  // Register types incrementally during checking via SIMD check patch
  // (types registered on-demand when isTypeRelatedTo is called)

  if (workerId === 0) {
    var gd = ts.getPreEmitDiagnostics(program).filter(function(d) { return !d.file; });
    for (var g = 0; g < gd.length; g++)
      output.push('error TS' + gd[g].code + ': ' + ts.flattenDiagnosticMessageText(gd[g].messageText, ' '));
  }

  // Wire Zig SIMD structural check into TSC's type checker
  // Types registered on-demand (first time each type is checked)
  if (typeof __edgebox_check_structural === 'function' && !globalThis.__zigCheckPatched) {
    var _checker = program.getDiagnosticsProducingTypeChecker ? program.getDiagnosticsProducingTypeChecker() : null;
    if (_checker && _checker.isTypeRelatedTo) {
      var _origCheck = _checker.isTypeRelatedTo.bind(_checker);
      var _regTypes = {};
      _checker.isTypeRelatedTo = function(source, target, relation) {
        if (source && target && source.id && target.id) {
          // Register types on-demand (zero pre-registration overhead)
          if (!_regTypes[source.id]) {
            _regTypes[source.id] = true;
            __edgebox_register_type(source.id, source.flags || 0);
          }
          if (!_regTypes[target.id]) {
            _regTypes[target.id] = true;
            __edgebox_register_type(target.id, target.flags || 0);
          }
          var r = __edgebox_check_structural(source.id, target.id);
          if (r === 1) return true;
          if (r === 0) return false;
        }
        return _origCheck(source, target, relation);
      };
    }
    globalThis.__zigCheckPatched = true;
  }

  for (var i = 0; i < files.length; i++) {
    if (i % workerCount !== workerId) continue;
    var diags = program.getSemanticDiagnostics(files[i]);
    for (var k = 0; k < diags.length; k++) {
      var d = diags[k];
      if (d.file) {
        var pos = d.file.getLineAndCharacterOfPosition(d.start || 0);
        output.push(d.file.fileName + '(' + (pos.line+1) + ',' + (pos.character+1) + '): error TS' + d.code + ': ' + ts.flattenDiagnosticMessageText(d.messageText, ' '));
      }
    }
  }

  return output.join(NL);
};
