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

  var t0 = Date.now();
  var cf = ts.readConfigFile(cwd + '/tsconfig.json', ts.sys.readFile);
  if (cf.error) return 'config error: ' + ts.flattenDiagnosticMessageText(cf.error.messageText, ' ');
  if (!cf.config) return 'config null';
  var parsed = ts.parseJsonConfigFileContent(cf.config, ts.sys, cwd);
  if (!globalThis.__pc) globalThis.__pc = {};
  var ck = cwd + ':' + parsed.fileNames.length;
  var t1 = Date.now();
  var program = globalThis.__pc[ck] || (globalThis.__pc[ck] = ts.createProgram(parsed.fileNames, parsed.options));
  var t2 = Date.now();
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
      var _regTypes = {};
      function _regType(t) {
        if (!t || !t.id || _regTypes[t.id]) return;
        _regTypes[t.id] = true;
        __edgebox_register_type(t.id, t.flags || 0);
        // Register union constituent types
        if (t.types && (t.flags & 1048576)) { // Union flag
          var ids = [];
          for (var u = 0; u < t.types.length; u++) {
            if (t.types[u] && t.types[u].id) ids.push(t.types[u].id);
          }
          if (ids.length > 0) __edgebox_register_union(t.id, ids);
        }
        // Register members (enables Zig structural comparison)
        try {
          var props = _checker.getPropertiesOfType(t);
          for (var p = 0; p < props.length && p < 50; p++) {
            var prop = props[p];
            if (prop.escapedName) {
              var pt = _checker.getTypeOfSymbol(prop);
              if (pt && pt.id) __edgebox_register_member(t.id, String(prop.escapedName), pt.id, prop.flags || 0);
            }
          }
        } catch(e) {}
      }
      globalThis.__zigHits = 0;
      globalThis.__zigMisses = 0;
      // Map relation object to integer for Zig (0=assignable, 1=subtype, 2=identity)
      var _assignableRel = ts.assignableRelation || null;
      var _subtypeRel = ts.subtypeRelation || null;
      _checker.isTypeRelatedTo = function(source, target, relation) {
        if (source && target && source.id && target.id) {
          _regType(source);
          _regType(target);
          // Pass relation type: 0=assignable, 1=subtype, 2=identity
          var relType = relation === _assignableRel ? 0 : relation === _subtypeRel ? 1 : 2;
          var r = __edgebox_check_structural(source.id, target.id);
          if (r === 1) { globalThis.__zigHits++; return true; }
          globalThis.__zigMisses++;
          return false;
        }
        return false;
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

  var t3 = Date.now();
  if (workerId === 0) {
    var zigHits = 0, zigMisses = 0;
    if (globalThis.__zigHits !== undefined) { zigHits = globalThis.__zigHits; zigMisses = globalThis.__zigMisses; }
    __edgebox_write_stderr('[recipe] w' + workerId + ' config:' + (t1-t0) + 'ms parse:' + (t2-t1) + 'ms check:' + (t3-t2) + 'ms total:' + (t3-t0) + 'ms types:' + Object.keys(_regTypes).length + ' zigHits:' + zigHits + ' zigMisses:' + zigMisses + String.fromCharCode(10));
  }
  return output.join(NL);
};
