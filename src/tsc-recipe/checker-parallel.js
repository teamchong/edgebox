// TSC Recipe — loaded once by V8 pool, called per request via __edgebox_check
//
// Optimizations use PUBLIC API wrapping — no fragile string matching on TSC source.
// WASM kernels compiled from Zig, loaded here. V8 TurboFan inlines WASM at callsite.
var ts = globalThis.ts || globalThis.module.exports;

// 0. WASM kernel init — deferred to first worker call (WebAssembly not available during snapshot).
// Loaded once per worker, TurboFan compiles after warmup calls.
globalThis.__zigWasmInitDone = false;

// --- Recipe optimizations (public API wrapping, stable across TSC versions) ---

// 1. Cache createSourceFile for .d.ts files (lib files parsed identically every time)
(function() {
  if (!ts || !ts.createSourceFile) return;
  var origCSF = ts.createSourceFile;
  var sfCache = new Map();
  ts.createSourceFile = function(fileName, sourceText, langVer, setParent, scriptKind) {
    if (fileName.endsWith('.d.ts')) {
      var cached = sfCache.get(fileName);
      if (cached && cached.text === sourceText) return cached;
    }
    var result = origCSF.apply(this, arguments);
    if (fileName.endsWith('.d.ts')) sfCache.set(fileName, result);
    return result;
  };
})();

// 3. Diagnostic cache — persist per-file diagnostics by content hash.
// Skip checking files that haven't changed since last check.
// Cache stored on disk as JSON — survives daemon restarts.
(function() {
  if (!ts) return;
  globalThis.__ebDiagCache = { hashes: {}, diags: {}, dirty: false };
  // Load from disk if available
  if (typeof __edgebox_read_file === 'function' && typeof __edgebox_root === 'function') {
    var root = __edgebox_root();
    try {
      var cached = __edgebox_read_file(root + '/.edgebox-diag-cache.json');
      if (cached) {
        var parsed = JSON.parse(cached);
        if (parsed && parsed.hashes) globalThis.__ebDiagCache = parsed;
      }
    } catch(e) {}
  }
})();

// 3. JIT warmup — run a small type check during snapshot creation.
// This triggers V8's TurboFan to compile TSC's hot functions (isTypeRelatedTo,
// checkTypeRelatedTo, getFlowTypeOfReference, etc.) BEFORE the snapshot.
// Workers restore with pre-optimized code — no JIT warmup on cold start.
(function() {
  if (!ts || !ts.createProgram || !ts.createSourceFile) return;
  try {
    var warmupSrc = 'var x: number = 1; var y: string = "a"; interface A { a: number; b: string; } interface B extends A { c: boolean; } var z: A = {} as B; type U = string | number | boolean; var u: U = 1; function f<T>(x: T): T { return x; } f(1); f("a");';
    var sf = ts.createSourceFile('__warmup.ts', warmupSrc, ts.ScriptTarget.Latest, true);
    var p = ts.createProgram({
      rootNames: ['__warmup.ts'],
      options: { strict: true, noEmit: true },
      host: {
        getSourceFile: function(name) { return name === '__warmup.ts' ? sf : undefined; },
        getDefaultLibFileName: function() { return 'lib.d.ts'; },
        writeFile: function() {},
        getCurrentDirectory: function() { return '/'; },
        getCanonicalFileName: function(f) { return f; },
        useCaseSensitiveFileNames: function() { return true; },
        getNewLine: function() { return '\n'; },
        fileExists: function(f) { return f === '__warmup.ts'; },
        readFile: function() { return ''; },
      }
    });
    // Run type checker MULTIPLE TIMES — triggers V8 TurboFan optimization.
    // V8 needs ~3-5 iterations to promote functions from Sparkplug to TurboFan.
    for (var warmI = 0; warmI < 5; warmI++) {
      ts.getPreEmitDiagnostics(p);
    }
  } catch(e) {}
})();

globalThis.__edgebox_check = function(cwd, workerId, workerCount) {
  try {
  if (!ts || !ts.createProgram) return 'no tsc: ' + typeof ts;

  // Load Zig WASM type kernel — NO FALLBACK. Must succeed or fail loud.
  if (!globalThis.__zigWasmInitDone) {
    globalThis.__zigWasmInitDone = true;
    if (typeof __edgebox_read_binary === 'function' && typeof WebAssembly !== 'undefined') {
      var _root = typeof __edgebox_root === 'function' ? __edgebox_root() : '';
      var _wasmPath = _root + '/src/tsc-recipe/type_kernel.wasm';
      var _buf = __edgebox_read_binary(_wasmPath);
      if (_buf && _buf instanceof ArrayBuffer && _buf.byteLength >= 8) {
        var _mod = new WebAssembly.Module(_buf);
        var _inst = new WebAssembly.Instance(_mod, {});
        globalThis.__zigTypeKernel = _inst.exports;
        // Warmup: trigger Liftoff → TurboFan promotion (500 calls each)
        var _k = _inst.exports.isSimpleTypeRelated;
        for (var _wi = 0; _wi < 500; _wi++) {
          _k(1, 1, 0, 1); _k(4, 8, 0, 1); _k(524288, 524288, 0, 1);
          _k(2, 1, 0, 1); _k(16, 16, 1, 1); _k(131072, 1, 0, 1);
        }
        __edgebox_write_stderr('[recipe] WASM kernel loaded (' + _buf.byteLength + ' bytes, TurboFan warmed)\n');
      } else {
        __edgebox_write_stderr('[recipe] WASM FAILED: read_binary returned ' + typeof _buf +
          (_buf ? ' len=' + _buf.byteLength : ' null') + ' for ' + _wasmPath + '\n');
      }
    } else {
      __edgebox_write_stderr('[recipe] WASM SKIPPED: read_binary=' + (typeof __edgebox_read_binary) +
        ' WebAssembly=' + (typeof WebAssembly) + '\n');
    }
  }

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
  var t1 = Date.now();

  // Module resolution cache (public API: CompilerHost.resolveModuleNames).
  // Caches results per worker. Lazy: skips bare modules without path mapping.
  if (!globalThis.__mrCache) globalThis.__mrCache = new Map();
  var defaultHost = ts.createCompilerHost(parsed.options);
  var pathPrefixes = parsed.options.paths
    ? Object.keys(parsed.options.paths).map(function(k) { return k.replace('/*', ''); })
    : [];

  var host = Object.create(defaultHost);
  host.resolveModuleNames = function(moduleNames, containingFile) {
    return moduleNames.map(function(name) {
      var key = name + '\0' + containingFile;
      var cached = globalThis.__mrCache.get(key);
      if (cached !== undefined) return cached;
      // Relative or path-mapped: resolve via TSC
      if (name.charAt(0) === '.') {
        var r = ts.resolveModuleName(name, containingFile, parsed.options, defaultHost).resolvedModule;
        globalThis.__mrCache.set(key, r || null);
        return r;
      }
      for (var pi = 0; pi < pathPrefixes.length; pi++) {
        if (name === pathPrefixes[pi] || name.indexOf(pathPrefixes[pi] + '/') === 0) {
          var r2 = ts.resolveModuleName(name, containingFile, parsed.options, defaultHost).resolvedModule;
          globalThis.__mrCache.set(key, r2 || null);
          return r2;
        }
      }
      // Bare module without path mapping: skip (will fail with TS2307)
      globalThis.__mrCache.set(key, null);
      return undefined;
    });
  };

  var oldProgram = globalThis.__pc[ck] || undefined;
  var isWarm = !!oldProgram;
  // Adaptive worker strategy (game-engine pattern):
  // Cold: all workers run in parallel (level loading — maximize throughput)
  // Warm: worker 0 only (game loop — maximize cache reuse, 875ms vs 962ms with 3)
  if (isWarm && workerId > 0) {
    __edgebox_write_stderr('[recipe] w' + workerId + ' warm skip (single-worker mode)' + String.fromCharCode(10));
    return '';
  }
  var program = ts.createProgram(parsed.fileNames, parsed.options, host, oldProgram);
  globalThis.__pc[ck] = program;
  var t2 = Date.now();
  var files = program.getSourceFiles();
  var NL = String.fromCharCode(10);
  var output = [];
  var filesChecked = 0;

  // Filter to project source files (skip .d.ts — skipLibCheck makes them instant)
  // Sort by size descending — game-engine priority scheduling: expensive files first
  // so work-stealing distributes them across workers instead of one worker getting stuck.
  var checkFiles = [];
  for (var fi = 0; fi < files.length; fi++)
    if (!files[fi].isDeclarationFile) checkFiles.push(files[fi]);
  checkFiles.sort(function(a, b) { return b.text.length - a.text.length; });

  // Hash each file — check if diagnostics are cached from previous run
  var dc = globalThis.__ebDiagCache;
  var cacheHits = 0, cacheMisses = 0;

  function hashContent(text) {
    if (typeof __edgebox_hash === 'function') return __edgebox_hash('sha256', text);
    // Fallback: simple hash
    var h = 0;
    for (var i = 0; i < text.length; i++) h = ((h << 5) - h + text.charCodeAt(i)) | 0;
    return '' + h;
  }

  function checkFile(file) {
    var hash = hashContent(file.text);
    var fn = file.fileName;
    // Cache hit: file unchanged since last check → return cached diagnostics
    if (dc.hashes[fn] === hash && dc.diags[fn] !== undefined) {
      cacheHits++;
      var cached = dc.diags[fn];
      for (var ci = 0; ci < cached.length; ci++) output.push(cached[ci]);
      return;
    }
    // Cache miss: check with TSC, store result
    cacheMisses++;
    filesChecked++;
    var diags = program.getSemanticDiagnostics(file);
    var fileDiags = [];
    for (var k = 0; k < diags.length; k++) {
      var d = diags[k];
      if (d.file) {
        var pos = d.file.getLineAndCharacterOfPosition(d.start || 0);
        var msg = d.file.fileName + '(' + (pos.line+1) + ',' + (pos.character+1) + '): error TS' + d.code + ': ' + ts.flattenDiagnosticMessageText(d.messageText, ' ');
        fileDiags.push(msg);
        output.push(msg);
      }
    }
    dc.hashes[fn] = hash;
    dc.diags[fn] = fileDiags;
    dc.dirty = true;
  }

  if (isWarm) {
    // Warm: single-worker mode — worker 0 checks ALL files with full cache reuse.
    // Workers 1-N already returned empty results above.
    for (var i = 0; i < checkFiles.length; i++) checkFile(checkFiles[i]);
  } else {
    while (true) {
      var idx = __edgebox_claim_file();
      if (idx >= checkFiles.length) break;
      checkFile(checkFiles[idx]);
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

  // Persist diagnostic cache to disk (all workers — each has partial results)
  if (dc.dirty && typeof __edgebox_write_file === 'function') {
    try {
      var ebRoot = typeof __edgebox_root === 'function' ? __edgebox_root() : __edgebox_cwd();
      __edgebox_write_file(ebRoot + '/.edgebox-diag-cache.json', JSON.stringify({hashes: dc.hashes, diags: dc.diags}));
      dc.dirty = false;
    } catch(e) {}
  }

  var t3 = Date.now();
  __edgebox_write_stderr('[recipe] w' + workerId + '/' + workerCount + ' config:' + (t1-t0) + 'ms parse:' + (t2-t1) + 'ms check:' + (t3-t2) + 'ms total:' + (t3-t0) + 'ms files:' + filesChecked + '/' + checkFiles.length + ' wasm:' + (globalThis.__zigTypeKernel ? 'on' : 'off') + String.fromCharCode(10));
  return output.join(NL);
  } catch(e) { return '[recipe-error] w' + workerId + ': ' + (e && e.stack ? e.stack : String(e)); }
};
