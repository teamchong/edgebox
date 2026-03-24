// TSC Recipe — loaded once by V8 pool, called per request via __edgebox_check
//
// Optimizations use PUBLIC API wrapping — no fragile string matching on TSC source.
// WASM kernels compiled from Zig, loaded here. V8 TurboFan inlines WASM at callsite.
var ts = globalThis.ts || globalThis.module.exports;

// 0. WasmGC — all WASM uses GC types (array.get/set, struct.get/set).
// V8 TurboFan inlines these at JS callsite. No linear memory WASM.
globalThis.__gcFlagsDone = false;

// --- Recipe optimizations (public API wrapping, stable across TSC versions) ---

// 0.5. Monomorphic Type constructor — eliminates V8 megamorphic IC (17% of runtime).
// TSC's default Type constructor only sets `this.flags`. Properties like `symbol`,
// `types`, `members` etc. are added dynamically, creating hundreds of hidden classes.
// Pre-initializing ALL properties in the constructor forces a single hidden class.
// Uses ts.setObjectAllocator (public API) — no source patching.
// Monomorphic Node constructor via ts.setObjectAllocator (public API).
// 19 pre-initialized props → single hidden class → V8 monomorphic IC.
// DATA: 1.70x parse speedup, 1.08x check speedup, 1.20x total.
(function() {
  if (!ts || !ts.setObjectAllocator || !ts.objectAllocator) { throw new Error('[recipe] FATAL: ts.setObjectAllocator not available'); }
  var origNodeProto = ts.objectAllocator.getNodeConstructor().prototype;
  function MonoNode(kind, pos, end) {
    this.pos = pos;
    this.end = end;
    this.kind = kind;
    this.id = 0;
    this.flags = 0;
    this.modifierFlagsCache = 0;
    this.transformFlags = 0;
    this.parent = void 0;
    this.symbol = void 0;
    this.locals = void 0;
    this.nextContainer = void 0;
    this.localSymbol = void 0;
    this.flowNode = void 0;
    this.emitNode = void 0;
    this.contextualType = void 0;
    this.inferenceContext = void 0;
    this.original = void 0;
    this.jsDoc = void 0;
    this.jsDocCache = void 0;
  }
  MonoNode.prototype = Object.create(origNodeProto);
  MonoNode.prototype.constructor = MonoNode;

  ts.setObjectAllocator({
    getNodeConstructor: function() { return MonoNode; },
    getTokenConstructor: function() { return MonoNode; },
    getIdentifierConstructor: function() { return MonoNode; },
    getPrivateIdentifierConstructor: function() { return MonoNode; },
    getSourceFileConstructor: ts.objectAllocator.getSourceFileConstructor,
    getSymbolConstructor: ts.objectAllocator.getSymbolConstructor,
    getTypeConstructor: ts.objectAllocator.getTypeConstructor,
    getSignatureConstructor: ts.objectAllocator.getSignatureConstructor,
    getSourceMapSourceConstructor: ts.objectAllocator.getSourceMapSourceConstructor,
  });
})();

// 1. Cache createSourceFile for .d.ts files (lib files parsed identically every time)
// .d.ts source file cache — shared between recipe wrapper and source injection.
// Source injection in createSourceFile reads globalThis.__sfCache.
// Pre-parse populates it. Both paths use the same Map.
var sfCache = new Map();
globalThis.__sfCache = sfCache;
(function() {
  if (!ts || !ts.createSourceFile) { throw new Error('[recipe] FATAL: ts.createSourceFile not available'); }
  var origCSF = ts.createSourceFile;
  globalThis.__sfCacheHits = 0;
  globalThis.__sfCacheMisses = 0;
  globalThis.__sfCacheSize = 0;
  ts.createSourceFile = function(fileName, sourceText, langVer, setParent, scriptKind) {
    if (fileName.endsWith('.d.ts')) {
      var cached = sfCache.get(fileName);
      if (cached) {
        if (cached.text === sourceText) { globalThis.__sfCacheHits++; return cached; }
        globalThis.__sfCacheMisses++;
      } else {
        globalThis.__sfCacheMisses++;
      }
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
    } catch(e) {
      if (typeof __edgebox_write_stderr === 'function')
        __edgebox_write_stderr('[recipe] diag cache load failed: ' + String(e) + '\n');
    }
  }
})();

// 3. Pre-parse lib .d.ts files during snapshot creation.
// These are the same for ALL projects. Parsing them here (573ms) means
// workers restore with pre-parsed ASTs — saves 573ms on cold start per worker.
// The createSourceFile cache (recipe #1) stores the results across requests too.
(function() {
  // REMOVED: pre-parse during snapshot. Tested:
  // - All 59 libs: +19MB snapshot, 300ms SLOWER (restore cost > parse savings)
  // - One lib (es5.d.ts): +400KB snapshot, ~20ms savings (marginal)
  // V8 snapshot preserves heap but NOT compiled code (Sparkplug/TurboFan).
  // Parse JIT warmup happens naturally during the first createProgram call.
  return;
  try {
    var root = __edgebox_root();
    var libDir = root + '/node_modules/typescript/lib/';
    // Parse the most commonly used lib files
    var libFiles = [
      'lib.es5.d.ts', 'lib.es2015.d.ts', 'lib.es2015.core.d.ts',
      'lib.es2015.collection.d.ts', 'lib.es2015.iterable.d.ts',
      'lib.es2015.generator.d.ts', 'lib.es2015.promise.d.ts',
      'lib.es2015.proxy.d.ts', 'lib.es2015.reflect.d.ts',
      'lib.es2015.symbol.d.ts', 'lib.es2015.symbol.wellknown.d.ts',
      'lib.es2016.d.ts', 'lib.es2016.array.include.d.ts',
      'lib.es2017.d.ts', 'lib.es2017.object.d.ts', 'lib.es2017.string.d.ts',
      'lib.es2017.intl.d.ts', 'lib.es2017.typedarrays.d.ts',
      'lib.es2017.sharedmemory.d.ts',
      'lib.es2018.d.ts', 'lib.es2018.asynciterable.d.ts',
      'lib.es2018.asyncgenerator.d.ts', 'lib.es2018.promise.d.ts',
      'lib.es2018.regexp.d.ts', 'lib.es2018.intl.d.ts',
      'lib.es2019.d.ts', 'lib.es2019.array.d.ts', 'lib.es2019.object.d.ts',
      'lib.es2019.string.d.ts', 'lib.es2019.symbol.d.ts',
      'lib.es2019.intl.d.ts',
      'lib.es2020.d.ts', 'lib.es2020.bigint.d.ts', 'lib.es2020.promise.d.ts',
      'lib.es2020.sharedmemory.d.ts', 'lib.es2020.string.d.ts',
      'lib.es2020.symbol.wellknown.d.ts', 'lib.es2020.intl.d.ts',
      'lib.es2020.date.d.ts', 'lib.es2020.number.d.ts',
      'lib.es2021.d.ts', 'lib.es2021.promise.d.ts',
      'lib.es2021.string.d.ts', 'lib.es2021.weakref.d.ts',
      'lib.es2021.intl.d.ts',
      'lib.es2022.d.ts', 'lib.es2022.array.d.ts', 'lib.es2022.error.d.ts',
      'lib.es2022.intl.d.ts', 'lib.es2022.object.d.ts',
      'lib.es2022.regexp.d.ts', 'lib.es2022.string.d.ts',
      'lib.es2022.sharedmemory.d.ts',
      'lib.es2023.d.ts', 'lib.es2023.array.d.ts',
      'lib.esnext.d.ts', 'lib.esnext.intl.d.ts',
      'lib.dom.d.ts', 'lib.dom.iterable.d.ts', 'lib.dom.asynciterable.d.ts',
    ];
    var count = 0;
    for (var i = 0; i < libFiles.length; i++) {
      var content = __edgebox_read_file(libDir + libFiles[i]);
      if (content) {
        // Parse and cache via the createSourceFile wrapper (recipe #1)
        var fullPath = libDir + libFiles[i];
        var sf = ts.createSourceFile(fullPath, content, 99 /* Latest */, true);
        sfCache.set(fullPath, sf);
        count++;
      }
    }
    if (typeof __edgebox_write_stderr === 'function')
      __edgebox_write_stderr('[recipe] pre-parsed ' + count + ' lib .d.ts, sfCache size=' + sfCache.size + ', lastSet=' + (globalThis.__lastSfCacheSet||'none') + '\n');
  } catch(e) {
    if (typeof __edgebox_write_stderr === 'function')
      __edgebox_write_stderr('[recipe] pre-parse error: ' + (e && e.stack ? e.stack : String(e)) + '\n');
  }
})();

globalThis.__edgebox_check = function(cwd, workerId, workerCount) {
  try {
  if (!ts || !ts.createProgram) return 'no tsc: ' + typeof ts;

  // Load WasmGC type flags module — V8 TurboFan inlines array.get/set at callsite.
  // Uses GC-managed arrays (not linear memory) for zero-overhead JS↔WASM access.
  if (!globalThis.__gcFlagsDone) {
    globalThis.__gcFlagsDone = true;
    if (typeof __edgebox_read_binary !== 'function' || typeof WebAssembly === 'undefined') {
      throw new Error('[recipe] FATAL: read_binary=' + (typeof __edgebox_read_binary) + ' WebAssembly=' + (typeof WebAssembly));
    }
    var _root = typeof __edgebox_root === 'function' ? __edgebox_root() : '';
    // 1. Load type_checker_gc.wasm — native WASM type comparison + flag storage
    var _checkerBuf = __edgebox_read_binary(_root + '/src/tsc-recipe/type_checker_gc.wasm');
    if (!_checkerBuf || !(_checkerBuf instanceof ArrayBuffer) || _checkerBuf.byteLength < 8)
      throw new Error('[recipe] FATAL: type_checker_gc.wasm read failed (' + _checkerBuf + ')');
    var _checkerInst = new WebAssembly.Instance(new WebAssembly.Module(_checkerBuf));
    globalThis.__gcChecker = _checkerInst.exports;
    globalThis.__gcFlags = _checkerInst.exports;
    globalThis.__gcFlagsArr = _checkerInst.exports.newFlags(65536);
    globalThis.__gcBloomArr = _checkerInst.exports.newFlags(65536);
    // 2. Load soa_gc.wasm — objectFlags array
    var _soaBuf = __edgebox_read_binary(_root + '/src/tsc-recipe/soa_gc.wasm');
    if (!_soaBuf || !(_soaBuf instanceof ArrayBuffer) || _soaBuf.byteLength < 8) {
      throw new Error('[recipe] FATAL: soa_gc.wasm read failed');
    }
    var _soaInst = new WebAssembly.Instance(new WebAssembly.Module(_soaBuf));
    globalThis.__gcSoa = _soaInst.exports;
    // Create objectFlags array (same capacity as type flags)
    globalThis.__gcObjFlagsArr = _soaInst.exports.newI32(65536);
    // Union member array: flat layout, 4 slots per type ID
    // [typeId*4] = count, [typeId*4+1..3] = member IDs (up to 3 members)
    // 16384 types * 4 = 65536 slots
    globalThis.__gcUnionArr = _checkerInst.exports.newFlags(65536);
    // 3. __gcCheck: thin wrapper → native WASM checkRelation + checkSrcToUnion
    var _checker = _checkerInst.exports.checkRelation;
    var _unionChecker = _checkerInst.exports.checkSrcToUnion;
    var _fA = globalThis.__gcFlagsArr, _bA = globalThis.__gcBloomArr;
    var _uA = globalThis.__gcUnionArr;
    globalThis.__gcCheck = function(_si, _ti) {
      var r = _checker(_fA, _bA, _si, _ti);
      if (r !== -1) return r;
      // Source → Target Union: check source against each union member
      if (_ti < 16384 && _si < 65536) {
        var sf = _checkerInst.exports.getFlag(_fA, _si | 0);
        var tf = _checkerInst.exports.getFlag(_fA, _ti | 0);
        if ((tf & 1048576) && !(sf & 3145728)) {
          var ur = _unionChecker(_fA, _bA, _uA, _si, _ti);
          if (ur !== -1) return ur;
        }
      }
      return -1;
    };
    // 4. Warmup + force TurboFan
    var _gf = _checkerInst.exports.getFlag, _sf = _checkerInst.exports.setFlag;
    for (var _wi = 0; _wi < 500; _wi++) {
      _sf(_fA, _wi % 100, _wi); _gf(_fA, _wi % 100);
      _checker(_fA, _bA, _wi % 100, (_wi + 1) % 100);
    }
    try {
      eval('%PrepareFunctionForOptimization(globalThis.__gcCheck)');
      globalThis.__gcCheck(1, 2); globalThis.__gcCheck(100, 200);
      eval('%OptimizeFunctionOnNextCall(globalThis.__gcCheck)');
      globalThis.__gcCheck(1, 2);
    } catch(e) {}
    __edgebox_write_stderr('[recipe] WasmGC: checker=' + _checkerBuf.byteLength + 'B soa=' + _soaBuf.byteLength + 'B\n');
  }
  if (!globalThis.__gcFlags) {
    throw new Error('[recipe] FATAL: WasmGC flags not loaded');
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

  // Create incremental program — uses tsbuildinfo to skip unchanged files.
  // First cold: full check + emit tsbuildinfo.
  // Second cold (daemon restart, no file changes): check = 0ms!
  // After file change: only re-checks changed files + dependents.
  if (!globalThis.__pc) globalThis.__pc = {};
  if (!globalThis.__cc) globalThis.__cc = {};
  if (!globalThis.__bp) globalThis.__bp = {}; // builder program cache
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
  // Incremental: only used for warm (in-memory builder). First cold uses createProgram.
  // createIncrementalProgram adds ~96ms overhead on first cold for no benefit.
  var useIncremental = !!globalThis.__bp[cwd + ':' + parsed.fileNames.length];
  if (useIncremental) {
    parsed.options.incremental = true;
    parsed.options.tsBuildInfoFile = cwd + '/.edgebox-tsbuildinfo';
  }
  var ck = cwd + ':' + parsed.fileNames.length;
  var t1 = Date.now();

  // Module resolution cache (public API: CompilerHost.resolveModuleNames).
  if (!globalThis.__mrCache) globalThis.__mrCache = new Map();
  var defaultHost = ts.createCompilerHost(parsed.options);
  var pathPrefixes = parsed.options.paths
    ? Object.keys(parsed.options.paths).map(function(k) { return k.replace('/*', ''); })
    : [];

  var host = Object.create(defaultHost);
  // Module resolution: Zig shared cache (across workers) + per-worker JS cache.
  // Worker 1 resolves a module → stores in Zig cache → workers 2+3 find it instantly.
  // Saves ~130ms per extra worker (191ms resolution × 2/3 hit rate).
  var hasNodeModules = __edgebox_dir_exists(cwd + '/node_modules') === 1;
  var hasZigResolveCache = typeof __edgebox_resolve_cache_get === 'function';
  host.resolveModuleNames = function(moduleNames, containingFile) {
    return moduleNames.map(function(name) {
      var key = name + '\0' + containingFile;
      // 1. Per-worker JS cache (fastest — same-worker repeat)
      var cached = globalThis.__mrCache.get(key);
      if (cached !== undefined) return cached;
      // 2. Zig shared cache (cross-worker)
      if (hasZigResolveCache) {
        var zigResult = __edgebox_resolve_cache_get(key);
        if (zigResult === -1) {
          globalThis.__mrCache.set(key, null);
          return undefined;
        }
        if (zigResult) {
          // Cache hit — still need TSC resolve for full ResolvedModule object,
          // but the Zig cache ensures fileExists calls hit Zig's file cache
          var r2 = ts.resolveModuleName(name, containingFile, parsed.options, defaultHost).resolvedModule;
          globalThis.__mrCache.set(key, r2 || null);
          return r2;
        }
      }
      // 3. Full resolution via TSC
      var shouldResolve = name.charAt(0) === '.';
      if (!shouldResolve) {
        for (var pi = 0; pi < pathPrefixes.length; pi++) {
          if (name === pathPrefixes[pi] || name.indexOf(pathPrefixes[pi] + '/') === 0) {
            shouldResolve = true; break;
          }
        }
      }
      if (!shouldResolve && hasNodeModules) shouldResolve = true;
      if (shouldResolve) {
        var r = ts.resolveModuleName(name, containingFile, parsed.options, defaultHost).resolvedModule;
        globalThis.__mrCache.set(key, r || null);
        // Store in Zig shared cache for other workers
        if (hasZigResolveCache) {
          if (r && r.resolvedFileName) {
            __edgebox_resolve_cache_set(key, r.resolvedFileName);
          } else {
            __edgebox_resolve_cache_set(key, '');
          }
        }
        return r;
      }
      globalThis.__mrCache.set(key, null);
      if (hasZigResolveCache) __edgebox_resolve_cache_set(key, '');
      return undefined;
    });
  };

  var oldProgram = globalThis.__pc[ck] || undefined;
  var isWarm = !!oldProgram;
  // Adaptive strategy:
  // Cold: all workers in parallel with createProgram (fastest parse).
  // Warm: worker 0 only with createIncrementalProgram (cache reuse + incremental check).
  if (isWarm && workerId > 0) {
    __edgebox_write_stderr('[recipe] w' + workerId + ' warm skip\n');
    return '';
  }
  var program, builder;
  if (isWarm && useIncremental) {
    // Warm: incremental program — only checks changed files
    var oldBuilder = globalThis.__bp[ck] || undefined;
    builder = ts.createIncrementalProgram({
      rootNames: parsed.fileNames,
      options: parsed.options,
      host: host,
      oldProgram: oldBuilder,
    });
    globalThis.__bp[ck] = builder;
    program = builder.getProgram();
  } else {
    // Cold: plain createProgram — fastest parse, no incremental overhead
    program = ts.createProgram(parsed.fileNames, parsed.options, host, oldProgram);
  }
  globalThis.__pc[ck] = program;
  var t2 = Date.now();
  var files = program.getSourceFiles();
  var NL = String.fromCharCode(10);
  var output = [];
  var filesChecked = 0;

  // Filter to project source files (skip .d.ts — skipLibCheck makes them instant)
  // Sort largest first — better work-stealing balance across workers.
  // Tested: large-first (4.4s) < small-first (4.8s) < no sort (5.1s).
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

  if (isWarm && builder) {
    // Warm+incremental: use builder's API — only checks changed files.
    var affected;
    while (affected = builder.getSemanticDiagnosticsOfNextAffectedFile()) {
      filesChecked++;
      var diags = affected.result;
      for (var k = 0; k < diags.length; k++) {
        var d = diags[k];
        if (d.file) {
          var pos = d.file.getLineAndCharacterOfPosition(d.start || 0);
          output.push(d.file.fileName + '(' + (pos.line+1) + ',' + (pos.character+1) + '): error TS' + d.code + ': ' + ts.flattenDiagnosticMessageText(d.messageText, ' '));
        }
      }
    }
  } else if (isWarm) {
    // Warm without incremental: check all files with cache reuse
    for (var i = 0; i < checkFiles.length; i++) checkFile(checkFiles[i]);
  } else {
    // Cold: parallel work-stealing. Skip diagnostic cache overhead on first cold —
    // empty cache means every hash + lookup is wasted work.
    var hasCachedDiags = Object.keys(dc.hashes).length > 0;
    while (true) {
      var idx = __edgebox_claim_file();
      if (idx >= checkFiles.length) break;
      if (hasCachedDiags) {
        checkFile(checkFiles[idx]);
      } else {
        // Direct check — no hash, no cache lookup, no cache store
        filesChecked++;
        var file = checkFiles[idx];
        var diags = program.getSemanticDiagnostics(file);
        for (var k = 0; k < diags.length; k++) {
          var d = diags[k];
          if (d.file) {
            var pos = d.file.getLineAndCharacterOfPosition(d.start || 0);
            output.push(d.file.fileName + '(' + (pos.line+1) + ',' + (pos.character+1) + '): error TS' + d.code + ': ' + ts.flattenDiagnosticMessageText(d.messageText, ' '));
          }
        }
      }
    }
  }

  // Worker 0: global diagnostics + emit tsbuildinfo for next incremental run
  if (workerId === 0) {
    var gd = (program.getGlobalDiagnostics ? program.getGlobalDiagnostics() : [])
      .concat(program.getOptionsDiagnostics ? program.getOptionsDiagnostics() : [])
      .concat(program.getConfigFileParsingDiagnostics ? program.getConfigFileParsingDiagnostics() : []);
    for (var g = 0; g < gd.length; g++) {
      if (!gd[g].file)
        output.push('error TS' + gd[g].code + ': ' + ts.flattenDiagnosticMessageText(gd[g].messageText, ' '));
    }
    // Emit tsbuildinfo — persists incremental state for warm runs.
    if (builder) try { builder.emit(); } catch(e) {}
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
  __edgebox_write_stderr('[recipe] w' + workerId + '/' + workerCount + ' parse:' + (t2-t1) + 'ms check:' + (t3-t2) + 'ms total:' + (t3-t0) + 'ms files:' + filesChecked + '/' + checkFiles.length + String.fromCharCode(10));
  return output.join(NL);
  } catch(e) { return '[recipe-error] w' + workerId + ': ' + (e && e.stack ? e.stack : String(e)); }
};
