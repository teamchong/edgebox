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

  // MonoType — pre-initialize ALL properties to force single hidden class.
  // V8 profile shows LoadIC_Megamorphic = 17% of runtime. Type objects are
  // the main culprit: default constructor only sets flags+checker, all other
  // properties added dynamically → hundreds of hidden class transitions.
  var origTypeProto = ts.objectAllocator.getTypeConstructor().prototype;
  // MonoType: WASM data model via public API (no source injection).
  // Constructor writes flags to WasmGC array using predicted type ID.
  // TSC's createType does: result = new Type(checker, flags); result.id = typeCount++;
  // We predict the ID by tracking our own counter (types are created sequentially).
  var _monoTypeCount = 0;
  var _monoFlagsArr = null; // set after WASM loads
  var _monoSetFlag = null;
  globalThis.__connectMonoType = function(flagsArr, setFlag) {
    _monoFlagsArr = flagsArr;
    _monoSetFlag = setFlag;
    _monoTypeCount = 0; // reset for each checker instance
  };
  function MonoType(checker, flags) {
    this.flags = flags;
    this.checker = checker;
    this.id = 0;
    this.objectFlags = 0;
    this.intrinsicName = void 0;
    this.debugIntrinsicName = void 0;
    this.symbol = void 0;
    this.immediateBaseConstraint = void 0;
    this.value = void 0;
    this.regularType = void 0;
    this.freshType = void 0;
    // Write flags to WASM array — no source injection needed.
    // TSC will set this.id = typeCount right after this constructor.
    // typeCount starts at 1 and increments by 1 per createType call.
    _monoTypeCount++;
    if (_monoSetFlag && _monoTypeCount < 131072) {
      _monoSetFlag(_monoFlagsArr, _monoTypeCount, flags | 0);
    }
  }
  MonoType.prototype = Object.create(origTypeProto);
  MonoType.prototype.constructor = MonoType;

  // MonoSignature — keep fat version (slimming hurt performance)
  var origSigProto = ts.objectAllocator.getSignatureConstructor().prototype;
  function MonoSignature(checker, flags) {
    this.flags = flags;
    this.checker = checker;
    this.parameters = void 0;
    this.declaration = void 0;
    this.typeParameters = void 0;
    this.resolvedReturnType = void 0;
    this.resolvedTypePredicate = void 0;
    this.minArgumentCount = 0;
    this.target = void 0;
    this.mapper = void 0;
    this.compositeSignatures = void 0;
    this.compositeKind = 0;
  }
  MonoSignature.prototype = Object.create(origSigProto);
  MonoSignature.prototype.constructor = MonoSignature;

  // MonoSymbol: hashes escapedName to i32 for WASM comparison.
  // Stored in _symbolNameHash[symbol.id] → WASM reads hash, compares as i32.
  var origSymProto = ts.objectAllocator.getSymbolConstructor().prototype;
  var _symbolNameHashArr = null;
  var _symbolSetFlag = null;
  var _symbolCount = 0;
  globalThis.__connectMonoSymbol = function(hashArr, setFlag) {
    _symbolNameHashArr = hashArr;
    _symbolSetFlag = setFlag;
    _symbolCount = 0;
  };
  function MonoSymbol(flags, name) {
    this.flags = flags;
    this.escapedName = name;
    this.declarations = void 0;
    this.valueDeclaration = void 0;
    this.id = 0;
    this.mergeId = 0;
    this.parent = void 0;
    this.members = void 0;
    this.exports = void 0;
    this.exportSymbol = void 0;
    this.constEnumOnlyModule = void 0;
    this.isReferenced = void 0;
    this.lastAssignmentPos = void 0;
    this.links = void 0;
    // Hash escapedName for WASM comparison
    if (_symbolSetFlag && name) {
      _symbolCount++;
      if (_symbolCount < 131072) {
        var h = 2166136261;
        for (var i = 0; i < name.length; i++) h = Math.imul(h ^ name.charCodeAt(i), 16777619);
        _symbolSetFlag(_symbolNameHashArr, _symbolCount, h | 0);
      }
    }
  }
  MonoSymbol.prototype = Object.create(origSymProto);
  MonoSymbol.prototype.constructor = MonoSymbol;

  ts.setObjectAllocator({
    getNodeConstructor: function() { return MonoNode; },
    getTokenConstructor: function() { return MonoNode; },
    getIdentifierConstructor: function() { return MonoNode; },
    getPrivateIdentifierConstructor: function() { return MonoNode; },
    getSourceFileConstructor: ts.objectAllocator.getSourceFileConstructor,
    getSymbolConstructor: function() { return MonoSymbol; },
    getTypeConstructor: function() { return MonoType; },
    getSignatureConstructor: function() { return MonoSignature; },
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

// Diagnostic cache — initialized lazily in __edgebox_check, not at snapshot time.
globalThis.__ebDiagCache = { hashes: {}, diags: {}, dirty: false };

globalThis.__edgebox_check = function(cwd, workerId, workerCount) {
  try {
  if (!ts || !ts.createProgram) return 'no tsc: ' + typeof ts;

  // Load WasmGC type flags module — V8 TurboFan inlines array.get/set at callsite.
  // Uses GC-managed arrays (not linear memory) for zero-overhead JS↔WASM access.
  if (!globalThis.__gcFlagsDone) {
    globalThis.__gcFlagsDone = true;
    if (typeof __edgebox_read_binary !== 'function' || typeof WebAssembly === 'undefined') {
      // WebAssembly not available (snapshot creation). Skip WASM loading.
      // createProgram still works — only type checking fast-paths need WASM.
      // Reset flag so workers can load WASM after snapshot restore.
      globalThis.__gcFlagsDone = false;
      __edgebox_write_stderr('[recipe] WASM not available (snapshot context) — skipping\n');
    } else {
    var _root = typeof __edgebox_root === 'function' ? __edgebox_root() : '';
    // 1. Load type_checker_gc.wasm — native WASM type comparison + flag storage
    var _checkerBuf = __edgebox_read_binary(_root + '/src/tsc-recipe/type_checker_gc.wasm');
    if (!_checkerBuf || !(_checkerBuf instanceof ArrayBuffer) || _checkerBuf.byteLength < 8)
      throw new Error('[recipe] FATAL: type_checker_gc.wasm read failed (' + _checkerBuf + ')');
    var _checkerInst = globalThis.__edgebox_instantiate_wasm
      ? globalThis.__edgebox_instantiate_wasm(_checkerBuf)
      : new WebAssembly.Instance(new WebAssembly.Module(_checkerBuf));
    globalThis.__gcChecker = _checkerInst.exports;
    globalThis.__gcFlags = _checkerInst.exports;
    // 131072 = 128K entries. Playwright creates ~76K types.
    globalThis.__gcFlagsArr = _checkerInst.exports.newFlags(131072);
    globalThis.__gcBloomArr = _checkerInst.exports.newFlags(131072);
    // 2. Load soa_gc.wasm — objectFlags array
    var _soaBuf = __edgebox_read_binary(_root + '/src/tsc-recipe/soa_gc.wasm');
    if (!_soaBuf || !(_soaBuf instanceof ArrayBuffer) || _soaBuf.byteLength < 8) {
      throw new Error('[recipe] FATAL: soa_gc.wasm read failed');
    }
    var _soaInst = globalThis.__edgebox_instantiate_wasm
      ? globalThis.__edgebox_instantiate_wasm(_soaBuf)
      : new WebAssembly.Instance(new WebAssembly.Module(_soaBuf));
    globalThis.__gcSoa = _soaInst.exports;
    // Create objectFlags array (same capacity as type flags)
    globalThis.__gcObjFlagsArr = _soaInst.exports.newI32(65536);
    // Union member array: flat layout, 4 slots per type ID
    // [typeId*4] = count, [typeId*4+1..3] = member IDs (up to 3 members)
    // 16384 types * 4 = 65536 slots
    globalThis.__gcUnionArr = _checkerInst.exports.newFlags(65536);
    // Property arrays for structural checking (WasmGC — TurboFan inlined)
    // [typeId*8+i] = property name hash / property type id
    // Max 8 properties per type (covers 90%+ of structural checks)
    // 16384 types * 8 = 131072 slots
    globalThis.__gcPropHashArr = _checkerInst.exports.newFlags(131072);
    globalThis.__gcPropTypeArr = _checkerInst.exports.newFlags(131072);
    globalThis.__gcPropCountArr = _checkerInst.exports.newFlags(16384);
    globalThis.__gcCheckStructural = _checkerInst.exports.checkStructural;
    // Connect WASM arrays to MonoType constructor (no source injection).
    if (globalThis.__connectMonoType) {
      globalThis.__connectMonoType(globalThis.__gcFlagsArr, _checkerInst.exports.setFlag);
    }
    // Symbol name hash array for WASM enum comparison
    globalThis.__gcSymbolHashArr = _checkerInst.exports.newFlags(131072);
    if (globalThis.__connectMonoSymbol) {
      globalThis.__connectMonoSymbol(globalThis.__gcSymbolHashArr, _checkerInst.exports.setFlag);
    }
    // Type value array for literal comparison (stores value as i32 hash)
    globalThis.__gcValueArr = _checkerInst.exports.newFlags(131072);
    // 3. __gcCheck: thin wrapper → native WASM checkRelation + checkSrcToUnion
    var _checker = _checkerInst.exports.checkRelation;
    var _unionChecker = _checkerInst.exports.checkSrcToUnion;
    var _fA = globalThis.__gcFlagsArr, _bA = globalThis.__gcBloomArr;
    var _uA = globalThis.__gcUnionArr;
    var _structChecker = _checkerInst.exports.checkStructural;
    var _pHA = globalThis.__gcPropHashArr;
    var _pTA = globalThis.__gcPropTypeArr;
    var _pCA = globalThis.__gcPropCountArr;
    // isRelatedToFast: combined identity + flags + union check — entirely in WASM.
    // Single call replaces the multi-step JS wrapper. No JS→WASM boundary overhead.
    var _relFast = _checkerInst.exports.isRelatedToFast;
    globalThis.__gcCheck = function(_si, _ti) {
      return _relFast(_fA, _bA, _uA, _si, _ti, 0, 0);
    };
    // Relation-aware: identity + flags + union + structural, all in one WASM call
    globalThis.__gcCheckRel = function(_si, _ti, _rel, _sn) {
      return _relFast(_fA, _bA, _uA, _si, _ti, _rel, _sn);
    };
    // WASM wrappers (__gcCheck, __gcCheckRel) created from exports of
    // __edgebox_instantiate_wasm — already have instant TurboFan.
    // Recipe-created wrappers also get force-optimized:
    if (globalThis.__edgebox_force_turbofan) {
      globalThis.__edgebox_force_turbofan(globalThis.__gcCheck);
      globalThis.__edgebox_force_turbofan(globalThis.__gcCheckRel);
    }
    __edgebox_write_stderr('[recipe] WasmGC: checker=' + _checkerBuf.byteLength + 'B soa=' + _soaBuf.byteLength + 'B\n');

    // 5. Load frozen isSimpleTypeRelatedTo — ACTUAL TSC function as WASM.
    // WASM imports call back to JS for enum/value/object checks.
    var _frozenBuf = __edgebox_read_binary(_root + '/src/tsc-recipe/frozen_checker.wasm');
    if (_frozenBuf && _frozenBuf instanceof ArrayBuffer && _frozenBuf.byteLength > 8) {
      // JS imports: called FROM WASM for closure dependencies.
      // Stubs until checker is created — then updated with real functions.
      // Import implementations for frozen checker.
      // enumCheck: for enum/value comparisons, return -1 (fallthrough to JS).
      // TSC's isEnumTypeRelatedTo is too complex to replicate here.
      globalThis.__frozenImportEnum = function(a, b) { return -1; };
      // objCheck: for Object→NonPrimitive strict subtype check, return -1 (fallthrough).
      globalThis.__frozenImportObj = function(a, b) { return -1; };
      // unknownCheck: isUnknownLikeUnionType — check if type is union of unknown.
      // Always return 0 (not unknown-like). This is safe — if TSC's JS finds it IS
      // unknown-like, it returns true anyway after the frozen check falls through.
      globalThis.__frozenImportUnknown = function(a) { return 0; };
      try {
        // Freeze-compiled WASM — no imports needed (pure numeric functions)
        var _frozenInst = globalThis.__edgebox_instantiate_wasm
          ? globalThis.__edgebox_instantiate_wasm(_frozenBuf)
          : new WebAssembly.Instance(new WebAssembly.Module(_frozenBuf));
        globalThis.__frozenIsRelated = _frozenInst.exports.isRelatedToFast;
        // __frozenIsRelated already force-TurboFan'd by __edgebox_instantiate_wasm
        __edgebox_write_stderr('[recipe] Frozen isRelatedToFast: ' + _frozenBuf.byteLength + 'B\n');
      } catch(e) {
        __edgebox_write_stderr('[recipe] Frozen load failed: ' + e.message + '\n');
      }
    }

    } // close else (WASM available)
  }
  // Test Zig parser + V8 bridge (if available)
  if (typeof __edgebox_zig_parse === 'function' && typeof globalThis.__zigCreateSourceFile === 'function') {
    var _zigTestSrc = 'const x: number = 1;\nlet y = x + 2;\n';
    var _zigAST = __edgebox_zig_parse(_zigTestSrc);
    if (_zigAST) {
      var _zigSF = globalThis.__zigCreateSourceFile(_zigTestSrc, 'test.ts', _zigAST);
      __edgebox_write_stderr('[recipe] Zig bridge: ' + (_zigAST.byteLength / 24) + ' flat nodes → ' + (_zigSF.statements ? _zigSF.statements.length : 0) + ' statements\n');
    }
  }

  // WASM may not be loaded during snapshot creation — that's OK.
  // Workers will load WASM on first __edgebox_check after snapshot restore.

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
  // Pre-program source file cache: if EDGEBOX_PROJECT snapshot created a program,
  // cache its source files so getSourceFile returns them instantly (skip parsing).
  // This eliminates 600ms of getSourceFile time per worker.
  if (globalThis.__preProgram && globalThis.__preProject === cwd) {
    var _preSfMap = new Map();
    var _preSFs = globalThis.__preProgram.getSourceFiles();
    for (var _psi = 0; _psi < _preSFs.length; _psi++) {
      _preSfMap.set(_preSFs[_psi].fileName, _preSFs[_psi]);
    }
    var _origHostGSF = defaultHost.getSourceFile;
    host.getSourceFile = function(fileName, languageVersionOrOptions, onError) {
      var cached = _preSfMap.get(fileName);
      if (cached) return cached;
      return _origHostGSF.call(this, fileName, languageVersionOrOptions, onError);
    };
    __edgebox_write_stderr('[recipe] w' + workerId + ' pre-program SF cache: ' + _preSfMap.size + ' files\n');
  }
  // Module resolution: Zig shared cache (across workers) + per-worker JS cache.
  // Worker 1 resolves a module → stores in Zig cache → workers 2+3 find it instantly.
  // Saves ~130ms per extra worker (191ms resolution × 2/3 hit rate).
  var hasNodeModules = __edgebox_dir_exists(cwd + '/node_modules') === 1;
  // Override getSourceFile to use Zig parser (when available and enabled)
  // Zig parser: opt-in via EDGEBOX_ZIG_PARSE env (bridge overhead still too high for net benefit)
  // When enabled: 447 files parsed by Zig (32ms) + bridge (880ms) = 912ms vs TSC's 750ms
  // Need: C++ bridge (create nodes in native) or lazy materialization to beat TSC
  // Zig parser: enabled for a SINGLE test file to identify forEachChild crash.
  // If it crashes, log the error and fall back to TSC for ALL files.
  // Zig parser disabled: 2024/2058 diags (34 missing from utilsBundle.ts require() handling).
  // Bridge improvements: operator tokens, ternary expressions, 40+ SyntaxKind mappings.
  // Remaining issues: typeof import() type annotations not treated as imports,
  // binder crash on destructuring assignment flow. Re-enable after these fixes.
  var useZigParser = false;
  var zigParseCount = 0, zigFallbackCount = 0;

  // Instrument forEachChild to detect infinite recursion
  if (useZigParser && ts.forEachChild) {
    var origFEC = ts.forEachChild;
    var fecDepth = 0;
    ts.forEachChild = function(node, cbNode, cbNodes) {
      fecDepth++;
      if (fecDepth > 500) {
        __edgebox_write_stderr('[HANG] forEachChild depth=' + fecDepth + ' kind=' + node.kind + ' pos=' + node.pos + '\n');
        fecDepth--;
        return undefined;
      }
      var result = origFEC(node, cbNode, cbNodes);
      fecDepth--;
      return result;
    };
  }

  if (useZigParser) {
    var origGetSourceFile = host.getSourceFile;
    host.getSourceFile = function(fileName, languageVersionOrOptions, onError) {
      // Use Zig for first N .ts files to isolate the hang
      // Binary search: find which file count causes hang
      // Zig for .d.ts files (type declarations — no errors, just type info).
      // TSC for .ts files (implementation — has errors, needs full AST).
      // .d.ts files are 88 files / 3.5MB = 140ms parse savings.
      // Zig for .ts files (implementation code).
      // TSC for .d.ts (type declarations — need complete export structure).
      // sf.imports pre-set prevents forEachChild hang.
      // ALL files parsed by Zig (57ms, no hang). 33/2058 diags — need more AST completeness.
      // Zig parser: check pre-loaded cache first
      if (globalThis.__zigSourceFileCache && globalThis.__zigSourceFileCache.has(fileName)) {
        zigParseCount++;
        return globalThis.__zigSourceFileCache.get(fileName);
      }
      zigFallbackCount++;
      return origGetSourceFile.call(this, fileName, languageVersionOrOptions, onError);
    };
  }

  var hasZigResolveCache = typeof __edgebox_resolve_cache_get === 'function';
  host.resolveModuleNames = function(moduleNames, containingFile) {
    return moduleNames.map(function(name) {
      var key = name + '\0' + containingFile;
      // 1. Per-worker JS cache (fastest — avoids all other lookups)
      var cached = globalThis.__mrCache.get(key);
      if (cached !== undefined) return cached;
      // 2. Zig shared resolve cache — replay worker 1's resolution in workers 2+3.
      // Saves 320ms: skip ts.resolveModuleName entirely, construct ResolvedModule directly.
      if (hasZigResolveCache) {
        var zigResult = __edgebox_resolve_cache_get(key);
        if (zigResult === -1) { globalThis.__mrCache.set(key, null); return undefined; }
        if (zigResult) {
          // Construct ResolvedModule with ALL fields TSC expects
          var ext = zigResult.endsWith('.d.ts') ? '.d.ts' :
                    zigResult.endsWith('.tsx') ? '.tsx' :
                    zigResult.endsWith('.ts') ? '.ts' :
                    zigResult.endsWith('.js') ? '.js' : '.ts';
          var resolved = {
            resolvedFileName: zigResult,
            originalPath: void 0,
            extension: ext,
            isExternalLibraryImport: zigResult.indexOf('/node_modules/') >= 0,
            packageId: void 0,
            resolvedUsingTsExtension: false
          };
          globalThis.__mrCache.set(key, resolved);
          return resolved;
        }
      }
      // 3. TSC resolution
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
        if (hasZigResolveCache) __edgebox_resolve_cache_set(key, r && r.resolvedFileName ? r.resolvedFileName : '');
        return r;
      }
      globalThis.__mrCache.set(key, null);
      if (hasZigResolveCache) __edgebox_resolve_cache_set(key, '');
      return undefined;
    });
  };

  // Check for pre-parsed program from project-specific snapshot.
  // __preProgram is set during snapshot creation (EDGEBOX_PROJECT env).
  // If the snapshot already ran getSemanticDiagnostics, the checker is fully warm —
  // we can SKIP createProgram entirely and reuse the pre-checked program directly.
  var oldProgram = globalThis.__pc[ck] || undefined;
  var usePreProgram = false;
  if (!oldProgram && globalThis.__preProgram && globalThis.__preProject === cwd) {
    oldProgram = globalThis.__preProgram;
    usePreProgram = true;
    __edgebox_write_stderr('[recipe] w' + workerId + ' using preProgram (' + globalThis.__preProgram.getSourceFiles().length + ' files)\n');
  }
  var isWarm = !!globalThis.__pc[ck]; // truly warm = from previous check, not from snapshot
  // Adaptive strategy:
  // Cold: all workers in parallel with createProgram (fastest parse).
  // Warm: worker 0 only with createIncrementalProgram (cache reuse + incremental check).
  if (isWarm && workerId > 0) {
    __edgebox_write_stderr('[recipe] w' + workerId + ' warm skip\n');
    return '';
  }
  // ── BATCH PRE-LOAD: Zig-parse ALL .ts files BEFORE createProgram ──
  // This is the key to speed: createProgram calls getSourceFile for each
  // imported file sequentially. If files are already cached, each call
  // returns instantly from the Map. No sequential Zig+bridge overhead.
  // Only worker 0 does batch pre-load — workers share nothing (separate isolates)
  // so each would redundantly parse all files. Worker 0 parses, others use TSC.
  __edgebox_write_stderr('[recipe] w' + workerId + ' batch-start useZig=' + useZigParser + '\n');
  if (useZigParser && !globalThis.__zigSourceFileCache && workerId === 0) {
    var cache = new Map();
    var t_zig0 = Date.now();
    __edgebox_write_stderr('[recipe] batch files: ' + parsed.fileNames.length + '\n');
    var tsFileCount = 0;
    for (var fi = 0; fi < parsed.fileNames.length; fi++) {
      var fn = parsed.fileNames[fi];
      if (fn.endsWith('.d.ts')) continue;
      tsFileCount++;
      var content = ts.sys.readFile(fn);
      if (content === undefined) continue;
      try {
        var _tp0 = Date.now();
        var flatAST = __edgebox_zig_parse(content);
        var _tp1 = Date.now();
        if (flatAST && flatAST.byteLength >= 24) {
          var sf = globalThis.__zigCreateSourceFile(content, fn, flatAST);
          var _tp2 = Date.now();
          if (_tp2 - _tp0 > 100) __edgebox_write_stderr('[SLOW] ' + fn.split('/').pop() + ' zig=' + (_tp1-_tp0) + 'ms bridge=' + (_tp2-_tp1) + 'ms\n');
          if (sf && sf.statements) {
            if (sf.imports) {
              for (var ii = 0; ii < sf.imports.length; ii++) {
                var imp = sf.imports[ii];
                if (imp.kind === 11 && !imp.text) {
                  imp.text = content.substring(imp.pos + 1, imp.end - 1);
                }
              }
            }
            // Validate: try forEachChild on each statement to catch bridge bugs early.
            // If any node has undefined children where TSC expects them, skip this file.
            var valid = true;
            try {
              for (var vi = 0; vi < sf.statements.length; vi++) {
                ts.forEachChild(sf.statements[vi], function(n) {
                  if (n) ts.forEachChild(n, function() {}); // 2-deep validation
                });
              }
            } catch(ve) { valid = false; }
            if (valid) cache.set(fn, sf);
          }
        }
      } catch(e) {}
      if (tsFileCount % 50 === 0) __edgebox_write_stderr('[recipe] batch ' + tsFileCount + ' ts files processed\n');
    }
    globalThis.__zigSourceFileCache = cache;
    __edgebox_write_stderr('[recipe] w' + workerId + ' batch Zig: ' + cache.size + ' files in ' + (Date.now() - t_zig0) + 'ms\n');
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
  } else if (false) {
    // DISABLED: skip-createProgram. Pre-program uses different host
    // (createCompilerHost vs recipe host) → wrong module resolution → wrong diags.
    program = globalThis.__preProgram;
  } else {
    // Cold: plain createProgram — time the phases
    var _cpT0 = Date.now();
    var _gsfCount = 0, _gsfTime = 0, _rmnCount = 0, _rmnTime = 0;
    var _origGSF = host.getSourceFile;
    host.getSourceFile = function() { var _t = Date.now(); _gsfCount++; var r = _origGSF.apply(this, arguments); _gsfTime += Date.now() - _t; return r; };
    if (host.resolveModuleNames) {
      var _origRMN = host.resolveModuleNames;
      host.resolveModuleNames = function() { var _t = Date.now(); _rmnCount++; var r = _origRMN.apply(this, arguments); _rmnTime += Date.now() - _t; return r; };
    }
    program = ts.createProgram(parsed.fileNames, parsed.options, host, oldProgram);
    __edgebox_write_stderr('[recipe] w' + workerId + ' createProgram=' + (Date.now()-_cpT0) + 'ms gsf=' + _gsfTime + 'ms(' + _gsfCount + ') rmn=' + _rmnTime + 'ms(' + _rmnCount + ')\n');
  }
  __edgebox_write_stderr('[recipe] POST-createProgram zig=' + zigParseCount + ' fb=' + zigFallbackCount + '\n');
  // Pre-initialize the TypeChecker — this creates intrinsic types, global symbols.
  // Without this, the FIRST getSemanticDiagnostics call pays ~300ms initialization.
  // getTypeChecker() is cheap if checker already exists.
  var _tc2 = program.getTypeChecker();
  // Activate freeze-compiled WASM for isSimpleTypeRelatedTo.
  // The frozen WASM takes integer flags — the wrapper extracts source.flags, target.flags.
  // Falls through to JS for cases WASM returns -1 (unknown).
  // Set JS fallback first (before WASM override)
  if (typeof __eb_isSimpleTypeRelatedTo_impl === 'function') {
    globalThis.__eb_isSimpleTypeRelatedTo = __eb_isSimpleTypeRelatedTo_impl;
  }
  // Override with freeze-compiled WASM (falls through to JS fallback for unknowns)
  // Frozen WASM disabled — false positives (44 diags). Need to fix WASM correctness.
  // Frozen WASM checker disabled — loading during snapshot corrupts warmup state.
  // The WasmGC isRelatedToFast (type_checker_gc.wasm) handles flag checks via
  // source injection. The freeze-compiled checker is redundant.
  // TODO: load frozen WASM only on workers (not during snapshot creation).
  // Activate other globals (force-TurboFan on JS impls — future: freeze to WASM)
  var _ebFuncs = ['isTypeRelatedTo'];
  for (var _ebi = 0; _ebi < _ebFuncs.length; _ebi++) {
    var _ebName = '__eb_' + _ebFuncs[_ebi];
    var _ebImpl = globalThis[_ebName + '_impl'];
    if (typeof _ebImpl === 'function') {
      globalThis[_ebName] = _ebImpl;
      if (globalThis.__edgebox_force_turbofan) {
        globalThis.__edgebox_force_turbofan(_ebImpl);
      }
    }
  }
  globalThis.__pc[ck] = program;
  if (!globalThis.__wildcardTypeId) globalThis.__wildcardTypeId = 0;


  var t2 = Date.now();
  __edgebox_write_stderr('[recipe] w' + workerId + ' zig=' + zigParseCount + ' fb=' + zigFallbackCount + '\n');
  var files = program.getSourceFiles();
  var NL = String.fromCharCode(10);
  var output = [];
  var filesChecked = 0;

  // Filter to project source files (skip .d.ts — skipLibCheck makes them instant)
  // Sort largest first — better work-stealing balance across workers.
  // Tested: large-first (4.4s) < small-first (4.8s) < no sort (5.1s).
  // Keep TSC's default file order (dependency-based) — better cache reuse.
  // Default order is 16% faster than largest-first for single-threaded check.
  // Work-stealing still distributes files across workers, just in dependency order.
  var checkFiles = [];
  for (var fi = 0; fi < files.length; fi++)
    if (!files[fi].isDeclarationFile) checkFiles.push(files[fi]);

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
    // Cold: parallel work-stealing.
    var hasCachedDiags = Object.keys(dc.hashes).length > 0;
    // Block work-stealing: claim 8 consecutive files at a time.
    // Balances cache locality (sequential checking) with load balancing.
    // Tested: block=4 (1.76s), block=8 (1.72s best), block=16 (1.76s),
    // static partition (1.83s — poor balance).
    var BLOCK = 8;
    while (true) {
      var blockStart = __edgebox_claim_file();
      if (blockStart >= checkFiles.length) break;
      for (var _bi = 1; _bi < BLOCK; _bi++) __edgebox_claim_file();
      var blockEnd = Math.min(blockStart + BLOCK, checkFiles.length);
      for (var idx = blockStart; idx < blockEnd; idx++) {
        if (hasCachedDiags) {
          checkFile(checkFiles[idx]);
        } else {
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
  var _gcInfo = '';
  if (globalThis.__getWasmStats) _gcInfo = ' ' + globalThis.__getWasmStats();
  __edgebox_write_stderr('[recipe] w' + workerId + '/' + workerCount + ' parse:' + (t2-t1) + 'ms check:' + (t3-t2) + 'ms total:' + (t3-t0) + 'ms files:' + filesChecked + '/' + checkFiles.length + _gcInfo + String.fromCharCode(10));
  return output.join(NL);
  } catch(e) { return '[recipe-error] w' + workerId + ': ' + (e && e.stack ? e.stack : String(e)); }
};
