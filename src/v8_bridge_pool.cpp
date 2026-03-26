// V8 Bridge for Pool — uses V8 headers for correct struct layout

#include <v8-isolate.h>
#include <v8-context.h>
#include <v8-script.h>
#include <v8-primitive.h>
#include <v8-local-handle.h>
#include <v8-value.h>
#include <v8-template.h>
#include <v8-function-callback.h>
#include <v8-exception.h>
#include <v8-persistent-handle.h>
#include <v8-function.h>
#include <v8-container.h>
#include <v8-typed-array.h>
#include <cstring>
#include <cstdio>
#include <string>
#include <vector>
#include <unistd.h>

// Forward declarations for callbacks used in g_external_refs
static void SfStorePutCallback(const v8::FunctionCallbackInfo<v8::Value>& args);
static void SfStoreGetCallback(const v8::FunctionCallbackInfo<v8::Value>& args);
static void SfStoreCountCallback(const v8::FunctionCallbackInfo<v8::Value>& args);
static void ZigCreateSourceFileCallback(const v8::FunctionCallbackInfo<v8::Value>& args);

// Flat AST node from Zig parser (24 bytes, extern struct)
struct FlatNode {
  uint16_t kind;
  uint16_t flags;
  uint32_t start;
  uint32_t end;
  uint32_t parent;
  uint32_t first_child;
  uint32_t next_sibling;
};

// Platform init via rusty_v8's C API (avoids libplatform dependency)
extern "C" {
  void* v8__Platform__NewDefaultPlatform(int thread_pool_size, bool idle_task_support);
  void v8__V8__InitializePlatform(void*);
  void v8__V8__Initialize();
  void v8__V8__SetFlagsFromString(const char* str, unsigned long len);
}

static void* g_platform = nullptr;
static bool g_initialized = false;

extern "C" {

void edgebox_v8_init() {
  if (g_initialized) return;
  // V8 flags for cold start performance:
  // - max-old-space-size: prevent OOM with multiple isolates
  // - concurrent-sparkplug: compile baseline JIT on background threads
  // - lazy-feedback-allocation: defer feedback vector allocation (less GC pressure)
  // - max-semi-space-size: larger young gen for type-heavy workloads
  // V8 flags optimized for cold start type checking:
  // --concurrent-sparkplug: baseline JIT on background threads
  // --max-semi-space-size=16: larger young gen reduces GC during type creation (47K types)
  // --turbo-inline-js-wasm-calls: enable TurboFan inlining of WasmGC functions into JS
  // V8 source: requires kInlineJSToWasmCalls flag + supported GC instructions only
  // V8 flags for TurboFan WasmGC inlining on cold start:
  // --turbo-inline-js-wasm-calls: enable WasmGC function inlining into JS
  // --allow-natives-syntax: enable %OptimizeFunctionOnNextCall to force TurboFan
  //   compilation of specific hot functions IMMEDIATELY (no 3000-call warmup)
  // --concurrent-sparkplug: baseline JIT on background threads
  // --max-semi-space-size=16: larger young gen reduces GC during type creation
  const char* extra_flags = getenv("EDGEBOX_V8_FLAGS");
  // --maglev: mid-tier JIT between Sparkplug and TurboFan — compiles fast, runs 2x faster than baseline
  // --invocation-count-for-maglev=30: lower threshold so checker functions hit Maglev on cold start
  // Tested: without maglev=2.53s, with maglev(30)=2.51s, maglev(10)=2.87s (too aggressive)
  // --max-semi-space-size=64: large young gen reduces GC pauses during type creation
  // Tested: 16=1.85s, 64=1.75s, 128=1.79s — 64MB is sweet spot
  const char* default_flags = "--max-old-space-size=4096 --concurrent-sparkplug --max-semi-space-size=64 --turbo-inline-js-wasm-calls --allow-natives-syntax --maglev --invocation-count-for-maglev=30";
  char flags_buf[1024];
  if (extra_flags && strlen(extra_flags) > 0) {
    snprintf(flags_buf, sizeof(flags_buf), "%s %s", default_flags, extra_flags);
  } else {
    snprintf(flags_buf, sizeof(flags_buf), "%s", default_flags);
  }
  const char* flags = flags_buf;
  v8__V8__SetFlagsFromString(flags, strlen(flags));
  // Platform threads for background JIT compilation (Sparkplug/TurboFan).
  // Tested: 2 = 3.1s (20% slower), 8 = 2.56s, 16 = 2.60s
  g_platform = v8__Platform__NewDefaultPlatform(8, false);
  v8__V8__InitializePlatform(g_platform);
  v8__V8__Initialize();
  g_initialized = true;
}

void* edgebox_v8_create_isolate() {
  v8::Isolate::CreateParams params;
  params.array_buffer_allocator = v8::ArrayBuffer::Allocator::NewDefaultAllocator();
  return v8::Isolate::New(params);
}

void edgebox_v8_enter_isolate(void* iso) {
  static_cast<v8::Isolate*>(iso)->Enter();
}

void edgebox_v8_exit_isolate(void* iso) {
  static_cast<v8::Isolate*>(iso)->Exit();
}

void edgebox_v8_dispose_isolate(void* iso) {
  static_cast<v8::Isolate*>(iso)->Dispose();
}

const char* edgebox_v8_eval(void* iso_ptr, const char* code, int code_len, int* out_len) {
  auto* isolate = static_cast<v8::Isolate*>(iso_ptr);
  v8::Isolate::Scope isolate_scope(isolate);
  v8::HandleScope handle_scope(isolate);
  auto context = v8::Context::New(isolate);
  v8::Context::Scope context_scope(context);

  auto source = v8::String::NewFromUtf8(isolate, code, v8::NewStringType::kNormal, code_len);
  if (source.IsEmpty()) { *out_len = 0; return nullptr; }

  auto script = v8::Script::Compile(context, source.ToLocalChecked());
  if (script.IsEmpty()) { *out_len = 0; return nullptr; }

  auto result = script.ToLocalChecked()->Run(context);
  if (result.IsEmpty()) { *out_len = 0; return nullptr; }

  v8::String::Utf8Value utf8(isolate, result.ToLocalChecked());
  if (*utf8 == nullptr) { *out_len = 0; return nullptr; }

  int len = utf8.length();
  char* copy = new char[len + 1];
  memcpy(copy, *utf8, len);
  copy[len] = '\0';
  *out_len = len;
  return copy;
}

void edgebox_v8_free(const char* ptr) {
  delete[] ptr;
}

// Forward declarations for callbacks (used in snapshot creation)
static void ReadFileCallback(const v8::FunctionCallbackInfo<v8::Value>& args);
static void ReadBinaryCallback(const v8::FunctionCallbackInfo<v8::Value>& args);
static void FileExistsCallback(const v8::FunctionCallbackInfo<v8::Value>& args);
static void DirExistsCallback(const v8::FunctionCallbackInfo<v8::Value>& args);
static void StatCallback(const v8::FunctionCallbackInfo<v8::Value>& args);
static void ReaddirCallback(const v8::FunctionCallbackInfo<v8::Value>& args);
static void RealpathCallback(const v8::FunctionCallbackInfo<v8::Value>& args);
static void CwdCallback(const v8::FunctionCallbackInfo<v8::Value>& args);
static void WriteStdoutCallback(const v8::FunctionCallbackInfo<v8::Value>& args);
static void WriteStderrCallback(const v8::FunctionCallbackInfo<v8::Value>& args);
static void HashCallback(const v8::FunctionCallbackInfo<v8::Value>& args);
static void ExitCallback(const v8::FunctionCallbackInfo<v8::Value>& args);
static void RootCallback(const v8::FunctionCallbackInfo<v8::Value>& args);
static void WriteFileCallback(const v8::FunctionCallbackInfo<v8::Value>& args);
static void ClaimFileCallback(const v8::FunctionCallbackInfo<v8::Value>& args);
static void SubmitResultCallback(const v8::FunctionCallbackInfo<v8::Value>& args);
static void WorkerDoneCallback(const v8::FunctionCallbackInfo<v8::Value>& args);
static void SignalProgramReadyCallback(const v8::FunctionCallbackInfo<v8::Value>& args);
static void WaitProgramReadyCallback(const v8::FunctionCallbackInfo<v8::Value>& args);
static void ResolveCacheGetCallback(const v8::FunctionCallbackInfo<v8::Value>& args);
static void ResolveCacheSetCallback(const v8::FunctionCallbackInfo<v8::Value>& args);
static void ResolveRelativeCallback(const v8::FunctionCallbackInfo<v8::Value>& args);
static void ZigParseCallback(const v8::FunctionCallbackInfo<v8::Value>& args);
// Dead stubs — type system migrated to WasmGC. Kept for external_refs compatibility.
static void NoopCallback(const v8::FunctionCallbackInfo<v8::Value>&) {}

// ── Shared Cache — cross-worker lock-free cache ──
extern "C" int edgebox_shared_cache_get(int key);
extern "C" void edgebox_shared_cache_set(int key, int value);
extern "C" void edgebox_shared_cache_clear();

static void SharedCacheGetCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  if (args.Length() < 1) { args.GetReturnValue().Set(0); return; }
  int key = args[0]->Int32Value(args.GetIsolate()->GetCurrentContext()).FromJust();
  args.GetReturnValue().Set(edgebox_shared_cache_get(key));
}

static void SharedCacheSetCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  if (args.Length() < 2) return;
  auto ctx = args.GetIsolate()->GetCurrentContext();
  int key = args[0]->Int32Value(ctx).FromJust();
  int value = args[1]->Int32Value(ctx).FromJust();
  edgebox_shared_cache_set(key, value);
}

// ── Snapshot: pre-compile TypeScript for instant worker startup ──

static v8::StartupData g_snapshot = {nullptr, 0};

// Helper: eval JS in a context (used during snapshot creation)
static void EvalInContext(v8::Isolate* isolate, v8::Local<v8::Context> context, const char* code, int len) {
  if (!code || len <= 0) return;
  auto source = v8::String::NewFromUtf8(isolate, code, v8::NewStringType::kNormal, len);
  if (source.IsEmpty()) return;
  auto script = v8::Script::Compile(context, source.ToLocalChecked());
  if (!script.IsEmpty()) script.ToLocalChecked()->Run(context);
}

// Forward declare Zig IO function
extern "C" void edgebox_write_stderr(const char* data, int len);

// V8 Code Cache: persists TurboFan-compiled code across runs.
// Created at build time after warmup. Workers load from cache = instant TurboFan.
static uint8_t* g_code_cache_data = nullptr;
static int g_code_cache_len = 0;

// TSC source stored for per-worker code-cache compilation.
static const char* g_ts_code = nullptr;
static int g_ts_code_len = 0;

// Create a snapshot with TypeScript + worker init + recipe pre-loaded.
// IO callbacks are registered on the snapshot context so worker_init can use real require('fs').
// External references — V8 needs these to serialize/deserialize native function pointers in snapshots
static const intptr_t g_external_refs[] = {
  reinterpret_cast<intptr_t>(ReadFileCallback),
  reinterpret_cast<intptr_t>(ReadBinaryCallback),
  reinterpret_cast<intptr_t>(FileExistsCallback),
  reinterpret_cast<intptr_t>(DirExistsCallback),
  reinterpret_cast<intptr_t>(StatCallback),
  reinterpret_cast<intptr_t>(ReaddirCallback),
  reinterpret_cast<intptr_t>(RealpathCallback),
  reinterpret_cast<intptr_t>(CwdCallback),
  reinterpret_cast<intptr_t>(WriteStdoutCallback),
  reinterpret_cast<intptr_t>(WriteStderrCallback),
  reinterpret_cast<intptr_t>(HashCallback),
  reinterpret_cast<intptr_t>(ExitCallback),
  reinterpret_cast<intptr_t>(NoopCallback), // was RegisterType — migrated to WasmGC
  reinterpret_cast<intptr_t>(NoopCallback), // was RegisterMember
  reinterpret_cast<intptr_t>(NoopCallback), // was RegisterUnion
  reinterpret_cast<intptr_t>(NoopCallback), // was CheckStructural
  reinterpret_cast<intptr_t>(ClaimFileCallback),
  reinterpret_cast<intptr_t>(NoopCallback), // was IOStats
  reinterpret_cast<intptr_t>(RootCallback),
  reinterpret_cast<intptr_t>(WriteFileCallback),
  reinterpret_cast<intptr_t>(NoopCallback), // was IsSimpleTypeRelated
  reinterpret_cast<intptr_t>(SubmitResultCallback),
  reinterpret_cast<intptr_t>(WorkerDoneCallback),
  reinterpret_cast<intptr_t>(SignalProgramReadyCallback),
  reinterpret_cast<intptr_t>(WaitProgramReadyCallback),
  reinterpret_cast<intptr_t>(ResolveCacheGetCallback),
  reinterpret_cast<intptr_t>(ResolveCacheSetCallback),
  reinterpret_cast<intptr_t>(ResolveRelativeCallback),
  reinterpret_cast<intptr_t>(ZigParseCallback),
  reinterpret_cast<intptr_t>(ZigCreateSourceFileCallback),
  reinterpret_cast<intptr_t>(SharedCacheGetCallback),
  reinterpret_cast<intptr_t>(SharedCacheSetCallback),
  reinterpret_cast<intptr_t>(SfStorePutCallback),
  reinterpret_cast<intptr_t>(SfStoreGetCallback),
  reinterpret_cast<intptr_t>(SfStoreCountCallback),
  0  // sentinel
};

int edgebox_v8_create_snapshot(const char* ts_code, int ts_len, const char* shim_code, int shim_len,
                                const char* init_code, int init_len, const char* recipe_code, int recipe_len) {
  v8::SnapshotCreator creator(g_external_refs);
  auto* isolate = creator.GetIsolate();
  {
    v8::Isolate::Scope isolate_scope(isolate);
    v8::HandleScope handle_scope(isolate);

    // Create context WITH IO callbacks (so worker_init's require('fs') works)
    auto global = v8::ObjectTemplate::New(isolate);
    global->Set(isolate, "__edgebox_read_file", v8::FunctionTemplate::New(isolate, ReadFileCallback));
    global->Set(isolate, "__edgebox_read_binary", v8::FunctionTemplate::New(isolate, ReadBinaryCallback));
    global->Set(isolate, "__edgebox_file_exists", v8::FunctionTemplate::New(isolate, FileExistsCallback));
    global->Set(isolate, "__edgebox_dir_exists", v8::FunctionTemplate::New(isolate, DirExistsCallback));
    global->Set(isolate, "__edgebox_stat", v8::FunctionTemplate::New(isolate, StatCallback));
    global->Set(isolate, "__edgebox_readdir", v8::FunctionTemplate::New(isolate, ReaddirCallback));
    global->Set(isolate, "__edgebox_realpath", v8::FunctionTemplate::New(isolate, RealpathCallback));
    global->Set(isolate, "__edgebox_cwd", v8::FunctionTemplate::New(isolate, CwdCallback));
    global->Set(isolate, "__edgebox_write_stdout", v8::FunctionTemplate::New(isolate, WriteStdoutCallback));
    global->Set(isolate, "__edgebox_write_stderr", v8::FunctionTemplate::New(isolate, WriteStderrCallback));
    global->Set(isolate, "__edgebox_hash", v8::FunctionTemplate::New(isolate, HashCallback));
    global->Set(isolate, "__edgebox_exit", v8::FunctionTemplate::New(isolate, ExitCallback));
    // Type system callbacks removed — migrated to WasmGC (type_flags_gc.wasm, soa_gc.wasm)
    global->Set(isolate, "__edgebox_claim_file", v8::FunctionTemplate::New(isolate, ClaimFileCallback));
    global->Set(isolate, "__edgebox_signal_program_ready", v8::FunctionTemplate::New(isolate, SignalProgramReadyCallback));
    global->Set(isolate, "__edgebox_wait_program_ready", v8::FunctionTemplate::New(isolate, WaitProgramReadyCallback));
    global->Set(isolate, "__edgebox_sf_store_put", v8::FunctionTemplate::New(isolate, SfStorePutCallback));
    global->Set(isolate, "__edgebox_sf_store_get", v8::FunctionTemplate::New(isolate, SfStoreGetCallback));
    global->Set(isolate, "__edgebox_sf_store_count", v8::FunctionTemplate::New(isolate, SfStoreCountCallback));
    global->Set(isolate, "__edgebox_root", v8::FunctionTemplate::New(isolate, RootCallback));
    global->Set(isolate, "__edgebox_resolve_cache_get", v8::FunctionTemplate::New(isolate, ResolveCacheGetCallback));
    global->Set(isolate, "__edgebox_resolve_cache_set", v8::FunctionTemplate::New(isolate, ResolveCacheSetCallback));
    global->Set(isolate, "__edgebox_resolve_relative", v8::FunctionTemplate::New(isolate, ResolveRelativeCallback));
    global->Set(isolate, "__edgebox_zig_parse", v8::FunctionTemplate::New(isolate, ZigParseCallback));
    global->Set(isolate, "__edgebox_zig_create_sf", v8::FunctionTemplate::New(isolate, ZigCreateSourceFileCallback));
    global->Set(isolate, "__edgebox_shared_cache_get", v8::FunctionTemplate::New(isolate, SharedCacheGetCallback));
    global->Set(isolate, "__edgebox_shared_cache_set", v8::FunctionTemplate::New(isolate, SharedCacheSetCallback));
    global->Set(isolate, "__edgebox_write_file", v8::FunctionTemplate::New(isolate, WriteFileCallback));
    // IsSimpleTypeRelated removed — migrated to WasmGC
    global->Set(isolate, "__edgebox_submit_result", v8::FunctionTemplate::New(isolate, SubmitResultCallback));
    global->Set(isolate, "__edgebox_worker_done", v8::FunctionTemplate::New(isolate, WorkerDoneCallback));
    auto context = v8::Context::New(isolate, nullptr, global);
    v8::Context::Scope context_scope(context);

    // 1. Eval shim (module, process stubs, require stubs)
    EvalInContext(isolate, context, shim_code, shim_len);
    // 2. Compile TypeScript via ScriptCompiler::CompileUnboundScript.
    // This gives us an UnboundScript we can create a code cache from AFTER warmup.
    // The code cache preserves TurboFan-compiled code (unlike snapshots which only keep bytecode).
    v8::Local<v8::UnboundScript> ts_unbound;
    {
      auto ts_str = v8::String::NewFromUtf8(isolate, ts_code, v8::NewStringType::kNormal, ts_len);
      if (!ts_str.IsEmpty()) {
        v8::ScriptOrigin origin(v8::String::NewFromUtf8Literal(isolate, "typescript.js"));
        v8::ScriptCompiler::Source source(ts_str.ToLocalChecked(), origin);
        auto maybe = v8::ScriptCompiler::CompileUnboundScript(isolate, &source);
        if (!maybe.IsEmpty()) {
          ts_unbound = maybe.ToLocalChecked();
          auto script = ts_unbound->BindToCurrentContext();
          script->Run(context).IsEmpty();
        }
      }
    }
    // 3. EdgeBox infrastructure: force TurboFan + WASM instantiation with auto-TurboFan.
    // These are EdgeBox APIs, not recipe-specific. Any recipe's WASM wrappers get instant TurboFan.
    {
      const char* infra_code =
          // Force instant TurboFan on a JS function.
          // Uses V8 %PrepareFunctionForOptimization + %OptimizeFunctionOnNextCall.
          "globalThis.__edgebox_force_turbofan = function(fn) {"
          "  try {"
          "    %PrepareFunctionForOptimization(fn);"
          "    %OptimizeFunctionOnNextCall(fn, 'concurrent');"
          "  } catch(e) {}"
          "};"
          // Instantiate a WASM module. WASM exports are already native —
          // they DON'T need %OptimizeFunctionOnNextCall (that crashes on WASM).
          // The JS WRAPPERS the recipe creates around WASM exports are what need TurboFan.
          // Recipe calls __edgebox_force_turbofan on its wrapper functions.
          "globalThis.__edgebox_instantiate_wasm = function(buf, imports) {"
          "  return new WebAssembly.Instance(new WebAssembly.Module(buf), imports);"
          "};";
      EvalInContext(isolate, context, infra_code, strlen(infra_code));
    }
    // 4. Set ts alias
    EvalInContext(isolate, context, "globalThis.ts = globalThis.module.exports;", 42);
    // 4. Eval worker_init (process, require with real IO)
    EvalInContext(isolate, context, init_code, init_len);
    // 5. Eval recipe (__edgebox_check defined)
    EvalInContext(isolate, context, recipe_code, recipe_len);

    // 6. Project-specific snapshot: pre-run createProgram during snapshot creation.
    //    EDGEBOX_PROJECT=/path/to/project → snapshot includes parsed program.
    //    Workers restore with program pre-loaded → skip 750ms parse.
    // Project-specific snapshot: pre-create program during snapshot.
    // Only createProgram (parse + bind) — NOT check. Keeps snapshot small.
    // Workers restore with parsed program → createProgram(oldProgram) reuses it.
    const char* project = getenv("EDGEBOX_PROJECT");
    if (project && strlen(project) > 0) {
      fprintf(stderr, "[v8pool] project snapshot: pre-parsing %s\n", project);
      char pre_parse[4096];
      snprintf(pre_parse, sizeof(pre_parse),
        "try {"
        "  if (!ts.sys && ts.setSys) ts.setSys({"
        "    args:[],newLine:'\\n',useCaseSensitiveFileNames:true,"
        "    write:function(){},writeOutputIsTTY:function(){return false;},"
        "    readFile:function(){return undefined;},writeFile:function(){},"
        "    fileExists:function(){return false;},directoryExists:function(){return false;},"
        "    createDirectory:function(){},getExecutingFilePath:function(){return '/';},"
        "    getCurrentDirectory:function(){return '/';},getDirectories:function(){return[];},"
        "    readDirectory:function(){return[];},exit:function(){},"
        "    realpath:function(p){return p;},getEnvironmentVariable:function(){return '';}"
        "  });"
        "  ts.sys.readFile = function(p) { return __edgebox_read_file(String(p)) || undefined; };"
        "  ts.sys.fileExists = function(p) { return __edgebox_file_exists(String(p)) === 1; };"
        "  ts.sys.directoryExists = function(p) { return __edgebox_dir_exists(String(p)) === 1; };"
        "  ts.sys.realpath = function(p) { return __edgebox_realpath(String(p)); };"
        "  ts.sys.getCurrentDirectory = function() { return '%s'; };"
        "  ts.sys.getDirectories = function(p) {"
        "    var j = __edgebox_readdir(String(p)); if(!j||j.charAt(0)!=='{')return[];"
        "    return JSON.parse(j).d||[]; };"
        "  ts.sys.readDirectory = function(root,ext,exc,inc,depth) {"
        "    return ts.matchFiles(root,ext,exc,inc,true,'%s',depth,function(p){"
        "      var j=__edgebox_readdir(p||'.');if(!j||j.charAt(0)!=='{')return{files:[],directories:[]};"
        "      var t=JSON.parse(j);return{files:t.f||[],directories:t.d||[]};},function(p){return __edgebox_realpath(p);});};"
        "  var _pp = ts.parseJsonConfigFileContent("
        "    ts.readConfigFile('%s/tsconfig.json', ts.sys.readFile).config, ts.sys, '%s');"
        "  var _host = ts.createCompilerHost(_pp.options);"
        "  var _ebRoot = typeof __edgebox_root === 'function' ? __edgebox_root() : '';"
        "  _host.getSourceFile = function(fn, lv, err) {"
        "    var c = ts.sys.readFile(fn);"
        "    if (c === undefined) {"
        "      var base = fn.split('/').pop();"
        "      if (base && base.indexOf('lib.') === 0 && base.endsWith('.d.ts'))"
        "        c = ts.sys.readFile(_ebRoot + '/node_modules/typescript/lib/' + base);"
        "    }"
        "    return c !== undefined ? ts.createSourceFile(fn, c, lv, true) : undefined;"
        "  };"
        "  var _prog = ts.createProgram(_pp.fileNames, _pp.options, _host);"
        "  globalThis.__preProgram = _prog;"
        "  globalThis.__preParsed = _pp;"
        "  globalThis.__preProject = '%s';"
        "  __edgebox_write_stderr('[snapshot] pre-parsed ' + _prog.getSourceFiles().length + ' files\\n');"
        // Warm up V8's type feedback by checking a few files.
        // This populates feedback vectors for ALL checker functions.
        // Workers restore with warm feedback → Maglev compiles on first call.
        // The checker STATE is discarded (workers create fresh programs).
        // Only the FEEDBACK VECTORS survive in the snapshot.
        "  var _tc3 = _prog.getTypeChecker();"
        "  var _warmFiles = _prog.getSourceFiles().filter(function(f){return !f.isDeclarationFile;});"
        "  var _warmN = Math.min(_warmFiles.length, 50);"
        "  for(var _wi=0;_wi<_warmN;_wi++) _prog.getSemanticDiagnostics(_warmFiles[_wi]);"
        "  __edgebox_write_stderr('[snapshot] warmed feedback on ' + _warmN + '/' + _warmFiles.length + ' files\\n');"
        "} catch(e) {"
        "  __edgebox_write_stderr('[snapshot] pre-parse failed: ' + e.message + '\\n');"
        "}", project, project, project, project, project);
      EvalInContext(isolate, context, pre_parse, strlen(pre_parse));
    }

    // Code cache must be created OUTSIDE snapshot creation (V8 limitation).
    // We'll create it in a separate isolate after the snapshot is done.
    creator.SetDefaultContext(context);
  }

  g_snapshot = creator.CreateBlob(v8::SnapshotCreator::FunctionCodeHandling::kKeep);

  // Code cache disabled — V8's CreateCodeCache only stores Sparkplug bytecode,
  // NOT TurboFan native code. The warmup overhead (~5s) isn't worth the ~30ms savings.
  // TurboFan is handled by __edgebox_force_turbofan at the recipe level.
  if (false && g_ts_code && g_ts_code_len > 0 && g_snapshot.data) {
    v8::Isolate::CreateParams params;
    params.array_buffer_allocator = v8::ArrayBuffer::Allocator::NewDefaultAllocator();
    params.snapshot_blob = &g_snapshot;
    params.external_references = g_external_refs;
    auto* cc_isolate = v8::Isolate::New(params);
    {
      v8::Isolate::Scope iso_scope(cc_isolate);
      v8::HandleScope handle_scope(cc_isolate);
      auto cc_context = v8::Context::New(cc_isolate);
      v8::Context::Scope ctx_scope(cc_context);

      // Register IO callbacks (snapshot context needs them for checker warmup)
      auto globalObj = cc_context->Global();
      auto setFn = [&](const char* name, v8::FunctionCallback cb) {
        globalObj->Set(cc_context,
          v8::String::NewFromUtf8(cc_isolate, name).ToLocalChecked(),
          v8::Function::New(cc_context, cb).ToLocalChecked()).Check();
      };
      setFn("__edgebox_read_file", ReadFileCallback);
      setFn("__edgebox_file_exists", FileExistsCallback);
      setFn("__edgebox_dir_exists", DirExistsCallback);
      setFn("__edgebox_readdir", ReaddirCallback);
      setFn("__edgebox_realpath", RealpathCallback);
      setFn("__edgebox_cwd", CwdCallback);
      setFn("__edgebox_write_stderr", WriteStderrCallback);
      setFn("__edgebox_write_stdout", WriteStdoutCallback);
      setFn("__edgebox_root", RootCallback);
      setFn("__edgebox_read_binary", ReadBinaryCallback);
      setFn("__edgebox_stat", StatCallback);
      setFn("__edgebox_hash", HashCallback);

      // Compile TSC via CompileUnboundScript, run it + warmup, then create code cache
      // from the SAME UnboundScript. This ensures TurboFan code is in the cache.
      auto ts_str = v8::String::NewFromUtf8(cc_isolate, g_ts_code,
          v8::NewStringType::kNormal, g_ts_code_len);
      if (!ts_str.IsEmpty()) {
        v8::ScriptOrigin origin(v8::String::NewFromUtf8Literal(cc_isolate, "typescript.js"));
        v8::ScriptCompiler::Source source(ts_str.ToLocalChecked(), origin);
        auto maybe = v8::ScriptCompiler::CompileUnboundScript(cc_isolate, &source,
            v8::ScriptCompiler::kEagerCompile);
        if (!maybe.IsEmpty()) {
          auto unbound = maybe.ToLocalChecked();
          // Run TSC (this second copy sets module.exports again)
          auto script = unbound->BindToCurrentContext();
          script->Run(cc_context).IsEmpty();
          EvalInContext(cc_isolate, cc_context,
              "globalThis.ts = globalThis.module.exports;", 42);

          // Run checker warmup using THIS TSC to trigger TurboFan
          const char* project = getenv("EDGEBOX_PROJECT");
          if (project && strlen(project) > 0) {
            char warmup[2048];
            snprintf(warmup, sizeof(warmup),
              "try {"
              "  var _p = globalThis.__preProgram;"
              "  if(!_p) { __edgebox_write_stderr('[code-cache] no preProgram\\n'); }"
              "  else {"
              "    var _tc = _p.getTypeChecker();"
              "    var _wf = _p.getSourceFiles().filter(function(f){return !f.isDeclarationFile;});"
              "    var _wn = Math.min(_wf.length, 100);"
              "    for(var _i=0;_i<_wn;_i++) _p.getSemanticDiagnostics(_wf[_i]);"
              "    __edgebox_write_stderr('[code-cache] warmed ' + _wn + ' files for TurboFan\\n');"
              "  }"
              "} catch(e) {"
              "  __edgebox_write_stderr('[code-cache] warmup err: ' + e.message + '\\n');"
              "}");
            EvalInContext(cc_isolate, cc_context, warmup, strlen(warmup));
          }

          // Create code cache from the SAME UnboundScript — includes TurboFan code
          auto* cache = v8::ScriptCompiler::CreateCodeCache(unbound);
          if (cache && cache->length > 0) {
            g_code_cache_len = cache->length;
            g_code_cache_data = new uint8_t[cache->length];
            memcpy(g_code_cache_data, cache->data, cache->length);
            char buf[128];
            int n = snprintf(buf, sizeof(buf), "[v8pool] code cache: %d bytes (TurboFan)\n",
                g_code_cache_len);
            edgebox_write_stderr(buf, n);
          }
          delete cache;
        }
      }
    }
    cc_isolate->Dispose();
    delete params.array_buffer_allocator;
  }

  return g_snapshot.raw_size;
}

// Create isolate from snapshot (instant TypeScript)
void* edgebox_v8_create_isolate_from_snapshot() {
  if (!g_snapshot.data) return edgebox_v8_create_isolate(); // fallback

  v8::Isolate::CreateParams params;
  params.array_buffer_allocator = v8::ArrayBuffer::Allocator::NewDefaultAllocator();
  params.snapshot_blob = &g_snapshot;
  params.external_references = g_external_refs;
  return v8::Isolate::New(params);
}

// Register IO globals on a context — connects V8 to Zig polyfills
// These are the same C ABI functions from edgebox_io.zig
extern const char* edgebox_read_file(const char* path, int path_len, int* out_len);
extern int edgebox_file_exists(const char* path, int path_len);
extern int edgebox_dir_exists(const char* path, int path_len);
extern const char* edgebox_stat(const char* path, int path_len, int* out_len);
extern const char* edgebox_readdir(const char* path, int path_len, int* out_len);
extern int edgebox_write_file(const char* path, int path_len, const char* data, int data_len);
extern const char* edgebox_realpath(const char* path, int path_len, int* out_len);
extern const char* edgebox_cwd(int* out_len);
extern void edgebox_write_stdout(const char* data, int len);
// edgebox_write_stderr declared at top of file with extern "C"
extern int edgebox_hash(const char* algo, int algo_len, const char* data, int data_len, char* out, int out_cap);
extern void edgebox_exit(int code);
extern void edgebox_free(const char* ptr, int len);

// Helper: get string arg from V8 callback
static std::string GetStringArg(const v8::FunctionCallbackInfo<v8::Value>& args, int idx) {
  if (idx >= args.Length()) return "";
  v8::String::Utf8Value utf8(args.GetIsolate(), args[idx]);
  return *utf8 ? std::string(*utf8, utf8.length()) : "";
}

// V8 callback: __edgebox_read_file(path) → string
static void ReadFileCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  if (args.Length() < 1) return;
  v8::String::Utf8Value utf8(args.GetIsolate(), args[0]);
  if (!*utf8) { args.GetReturnValue().Set(v8::String::NewFromUtf8(args.GetIsolate(), "").ToLocalChecked()); return; }
  int out_len = 0;
  auto* data = edgebox_read_file(*utf8, utf8.length(), &out_len);
  if (!data || out_len <= 0) {
    args.GetReturnValue().Set(v8::String::NewFromUtf8(args.GetIsolate(), "").ToLocalChecked());
    return;
  }
  args.GetReturnValue().Set(
    v8::String::NewFromUtf8(args.GetIsolate(), data, v8::NewStringType::kNormal, out_len).ToLocalChecked());
}

// Read binary file → Uint8Array (for WASM modules, binary assets)
// Uses Uint8Array wrapping an ArrayBuffer so JS sees proper binary data.
static void ReadBinaryCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  if (args.Length() < 1) return;
  auto* isolate = args.GetIsolate();
  v8::HandleScope scope(isolate);
  v8::String::Utf8Value utf8(isolate, args[0]);
  if (!*utf8) return;
  int out_len = 0;
  auto* data = edgebox_read_file(*utf8, utf8.length(), &out_len);
  if (!data || out_len <= 0) return;
  // Use Uint8Array approach — JS can pass .buffer to WebAssembly.Module
  auto context = isolate->GetCurrentContext();
  auto ab = v8::ArrayBuffer::New(isolate, (size_t)out_len);
  if (ab.IsEmpty()) { fprintf(stderr, "[v8] ReadBinary: ArrayBuffer::New failed\n"); return; }
  // Get the data pointer and copy
  void* dest = ab->Data();
  if (!dest) { fprintf(stderr, "[v8] ReadBinary: ab->Data() null\n"); return; }
  memcpy(dest, data, out_len);
  args.GetReturnValue().Set(ab);
}

static void FileExistsCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  if (args.Length() < 1) return;
  v8::String::Utf8Value utf8(args.GetIsolate(), args[0]);
  if (!*utf8) { args.GetReturnValue().Set(v8::Number::New(args.GetIsolate(), 0)); return; }
  args.GetReturnValue().Set(v8::Number::New(args.GetIsolate(), edgebox_file_exists(*utf8, utf8.length())));
}

static void DirExistsCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  if (args.Length() < 1) return;
  v8::String::Utf8Value utf8(args.GetIsolate(), args[0]);
  if (!*utf8) { args.GetReturnValue().Set(v8::Number::New(args.GetIsolate(), 0)); return; }
  args.GetReturnValue().Set(v8::Number::New(args.GetIsolate(), edgebox_dir_exists(*utf8, utf8.length())));
}

static void StatCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  auto path = GetStringArg(args, 0);
  int out_len = 0;
  auto* data = edgebox_stat(path.c_str(), path.size(), &out_len);
  auto* iso = args.GetIsolate();
  if (!data || out_len <= 0) {
    args.GetReturnValue().Set(v8::String::NewFromUtf8(iso, "").ToLocalChecked());
    return;
  }
  // stat uses thread-local buffer — do NOT free
  auto result = v8::String::NewFromUtf8(iso, data, v8::NewStringType::kNormal, out_len).ToLocalChecked();
  args.GetReturnValue().Set(result);
}

static void ReaddirCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  auto path = GetStringArg(args, 0);
  int out_len = 0;
  auto* data = edgebox_readdir(path.c_str(), path.size(), &out_len);
  auto* iso = args.GetIsolate();
  if (!data || out_len <= 0) {
    args.GetReturnValue().Set(v8::String::NewFromUtf8(iso, "[]").ToLocalChecked());
    return;
  }
  auto result = v8::String::NewFromUtf8(iso, data, v8::NewStringType::kNormal, out_len).ToLocalChecked();
  edgebox_free(data, out_len);
  args.GetReturnValue().Set(result);
}

static void RealpathCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  auto path = GetStringArg(args, 0);
  int out_len = 0;
  auto* data = edgebox_realpath(path.c_str(), path.size(), &out_len);
  auto* iso = args.GetIsolate();
  if (!data || out_len <= 0) {
    args.GetReturnValue().Set(args[0]);
    return;
  }
  auto result = v8::String::NewFromUtf8(iso, data, v8::NewStringType::kNormal, out_len).ToLocalChecked();
  edgebox_free(data, out_len);
  args.GetReturnValue().Set(result);
}

static void CwdCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  int out_len = 0;
  auto* data = edgebox_cwd(&out_len);
  auto* iso = args.GetIsolate();
  if (!data) { args.GetReturnValue().Set(v8::String::NewFromUtf8(iso, "/").ToLocalChecked()); return; }
  auto result = v8::String::NewFromUtf8(iso, data, v8::NewStringType::kNormal, out_len).ToLocalChecked();
  edgebox_free(data, out_len);
  args.GetReturnValue().Set(result);
}

static void WriteStdoutCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  auto str = GetStringArg(args, 0);
  edgebox_write_stdout(str.c_str(), str.size());
}

static void WriteStderrCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  auto str = GetStringArg(args, 0);
  edgebox_write_stderr(str.c_str(), str.size());
}

static void HashCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  auto algo = GetStringArg(args, 0);
  auto data = GetStringArg(args, 1);
  char out[129];
  int len = edgebox_hash(algo.c_str(), algo.size(), data.c_str(), data.size(), out, 128);
  if (len <= 0) { args.GetReturnValue().Set(v8::String::NewFromUtf8(args.GetIsolate(), "").ToLocalChecked()); return; }
  out[len] = '\0';
  args.GetReturnValue().Set(v8::String::NewFromUtf8(args.GetIsolate(), out, v8::NewStringType::kNormal, len).ToLocalChecked());
}

static void ExitCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  (void)args;
}

// Type system: register types/members in Zig flat arrays for SIMD check
// Type system externs removed — migrated to WasmGC
extern const char* edgebox_root(int* out_len);
extern unsigned int edgebox_claim_file();
extern void edgebox_reset_work();

static void WriteFileCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  if (args.Length() < 2) return;
  auto path = GetStringArg(args, 0);
  auto data = GetStringArg(args, 1);
  edgebox_write_file(path.c_str(), path.size(), data.c_str(), data.size());
}

static void RootCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  int out_len = 0;
  auto* data = edgebox_root(&out_len);
  if (!data || out_len <= 0) { args.GetReturnValue().Set(v8::String::NewFromUtf8(args.GetIsolate(), "/").ToLocalChecked()); return; }
  args.GetReturnValue().Set(v8::String::NewFromUtf8(args.GetIsolate(), data, v8::NewStringType::kNormal, out_len).ToLocalChecked());
}

static void ClaimFileCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  args.GetReturnValue().Set(v8::Number::New(args.GetIsolate(), edgebox_claim_file()));
}

extern void edgebox_signal_program_ready();
extern void edgebox_wait_program_ready();

static void SignalProgramReadyCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  edgebox_signal_program_ready();
}

static void WaitProgramReadyCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  edgebox_wait_program_ready();
}

// ── Shared SourceFile Store (worker 0 → workers 1-N) ──
extern "C" void edgebox_sf_store_put(const char* key, int key_len, const char* val, int val_len);
extern "C" const char* edgebox_sf_store_get(const char* key, int key_len, int* out_len);
extern "C" void edgebox_sf_store_clear();
extern "C" unsigned int edgebox_sf_store_count();

static void SfStorePutCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  // JS: __edgebox_sf_store_put(fileName, arrayBuffer)
  if (args.Length() < 2) return;
  auto* isolate = args.GetIsolate();
  v8::String::Utf8Value key(isolate, args[0]);
  if (!*key) return;
  if (args[1]->IsArrayBuffer()) {
    auto ab = args[1].As<v8::ArrayBuffer>();
    auto store = ab->GetBackingStore();
    edgebox_sf_store_put(*key, key.length(), (const char*)store->Data(), store->ByteLength());
  } else if (args[1]->IsUint8Array()) {
    auto u8arr = args[1].As<v8::Uint8Array>();
    auto ab = u8arr->Buffer();
    auto store = ab->GetBackingStore();
    edgebox_sf_store_put(*key, key.length(),
      (const char*)store->Data() + u8arr->ByteOffset(), u8arr->ByteLength());
  }
}

static void SfStoreGetCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  // JS: __edgebox_sf_store_get(fileName) → ArrayBuffer | undefined
  if (args.Length() < 1) return;
  auto* isolate = args.GetIsolate();
  v8::String::Utf8Value key(isolate, args[0]);
  if (!*key) return;
  int out_len = 0;
  const char* data = edgebox_sf_store_get(*key, key.length(), &out_len);
  if (data && out_len > 0) {
    // Create ArrayBuffer with copy of data (each isolate needs its own copy)
    auto ab = v8::ArrayBuffer::New(isolate, out_len);
    memcpy(ab->GetBackingStore()->Data(), data, out_len);
    args.GetReturnValue().Set(ab);
  }
}

static void SfStoreCountCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  args.GetReturnValue().Set(v8::Number::New(args.GetIsolate(), edgebox_sf_store_count()));
}

extern void edgebox_submit_result(int worker_id, const char* data, int data_len);
extern void edgebox_worker_done(int worker_id);

static void SubmitResultCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  if (args.Length() < 2) return;
  int wid = args[0]->Int32Value(args.GetIsolate()->GetCurrentContext()).FromMaybe(0);
  auto str = GetStringArg(args, 1);
  edgebox_submit_result(wid, str.c_str(), str.size());
}

static void WorkerDoneCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  if (args.Length() < 1) return;
  int wid = args[0]->Int32Value(args.GetIsolate()->GetCurrentContext()).FromMaybe(0);
  edgebox_worker_done(wid);
}

// Module resolution cache — shared across workers via Zig
extern "C" int edgebox_resolve_cache_get(const char* key, int key_len, const char** out_ptr, int* out_len);
extern "C" void edgebox_resolve_cache_set(const char* key, int key_len, const char* path, int path_len);

static void ResolveCacheGetCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  // JS: __edgebox_resolve_cache_get(key) → string (hit) | -1 (cached fail) | undefined (miss)
  if (args.Length() < 1) return;
  auto* isolate = args.GetIsolate();
  v8::String::Utf8Value key(isolate, args[0]);
  if (!*key) return;
  const char* out_ptr = nullptr;
  int out_len = 0;
  int result = edgebox_resolve_cache_get(*key, key.length(), &out_ptr, &out_len);
  if (result == 1 && out_ptr && out_len > 0) {
    // Hit — return resolved path
    args.GetReturnValue().Set(v8::String::NewFromUtf8(isolate, out_ptr, v8::NewStringType::kNormal, out_len).ToLocalChecked());
  } else if (result == -1) {
    // Cached failure
    args.GetReturnValue().Set(v8::Integer::New(isolate, -1));
  }
  // result == 0: miss — return undefined (default)
}

static void ResolveCacheSetCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  // JS: __edgebox_resolve_cache_set(key, path)
  if (args.Length() < 2) return;
  auto* isolate = args.GetIsolate();
  v8::String::Utf8Value key(isolate, args[0]);
  v8::String::Utf8Value path(isolate, args[1]);
  if (!*key) return;
  edgebox_resolve_cache_set(*key, key.length(), *path ? *path : "", *path ? path.length() : 0);
}

extern "C" int edgebox_resolve_relative(const char* import_name, int import_len, const char* dir, int dir_len, const char** out_ptr, int* out_len);
extern "C" int edgebox_zig_parse(const char* src, int src_len, const char** out_nodes, int* out_count);

static void ResolveRelativeCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  // JS: __edgebox_resolve_relative(importName, containingDir) → string | undefined
  if (args.Length() < 2) return;
  auto* isolate = args.GetIsolate();
  v8::String::Utf8Value import_name(isolate, args[0]);
  v8::String::Utf8Value dir(isolate, args[1]);
  if (!*import_name || !*dir) return;
  const char* out_ptr = nullptr;
  int out_len = 0;
  int result = edgebox_resolve_relative(*import_name, import_name.length(), *dir, dir.length(), &out_ptr, &out_len);
  if (result == 1 && out_ptr && out_len > 0) {
    args.GetReturnValue().Set(v8::String::NewFromUtf8(isolate, out_ptr, v8::NewStringType::kNormal, out_len).ToLocalChecked());
  }
}

static void ZigParseCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  // Mode 1: __edgebox_zig_parse(sourceText) → ArrayBuffer of flat AST nodes
  // Mode 2: __edgebox_zig_parse(sourceText, fileName) → SourceFile object (C++ bridge)
  if (args.Length() < 1) return;
  auto* isolate = args.GetIsolate();
  v8::String::Utf8Value source(isolate, args[0]);
  if (!*source) return;
  const char* out_nodes = nullptr;
  int out_count = 0;
  int ok = edgebox_zig_parse(*source, source.length(), &out_nodes, &out_count);
  if (ok != 1 || !out_nodes || out_count <= 0) return;

  if (args.Length() < 2 || !args[1]->IsString()) {
    // Mode 1: return ArrayBuffer (backward compat)
    size_t byte_len = (size_t)out_count * 24;
    auto ab = v8::ArrayBuffer::New(isolate, byte_len);
    if (ab->Data()) memcpy(ab->Data(), out_nodes, byte_len);
    args.GetReturnValue().Set(ab);
    return;
  }

  // Mode 2: create SourceFile directly in C++
  auto context = isolate->GetCurrentContext();
  const FlatNode* nodes = reinterpret_cast<const FlatNode*>(out_nodes);
  int nodeCount = out_count;

  // For very large files (>50K nodes), fall back to returning ArrayBuffer
  // to avoid V8 heap pressure from creating too many objects at once
  if (nodeCount > 50000) {
    size_t bl = (size_t)nodeCount * 24;
    auto ab = v8::ArrayBuffer::New(isolate, bl);
    auto bs = ab->GetBackingStore();
    if (bs && bs->Data()) memcpy(bs->Data(), out_nodes, bl);
    args.GetReturnValue().Set(ab);
    return;
  }

  auto global = context->Global();
  auto tsVal = global->Get(context, v8::String::NewFromUtf8Literal(isolate, "ts")).ToLocalChecked();
  if (!tsVal->IsObject()) { edgebox_write_stderr("[cpp-bridge] ts not found\n", 26); return; }
  auto ts = tsVal.As<v8::Object>();
  auto allocVal = ts->Get(context, v8::String::NewFromUtf8Literal(isolate, "objectAllocator")).ToLocalChecked();
  if (!allocVal->IsObject()) return;
  auto alloc_obj = allocVal.As<v8::Object>();
  auto getNodeCtorVal = alloc_obj->Get(context, v8::String::NewFromUtf8Literal(isolate, "getNodeConstructor")).ToLocalChecked();
  if (!getNodeCtorVal->IsFunction()) return;
  auto NodeCtor = getNodeCtorVal.As<v8::Function>()->Call(context, alloc_obj, 0, nullptr).ToLocalChecked().As<v8::Function>();

  auto s_flags = v8::String::NewFromUtf8Literal(isolate, "flags");
  auto s_parent = v8::String::NewFromUtf8Literal(isolate, "parent");

  // Create ALL nodes in C++ (no JS loop)
  std::vector<v8::Local<v8::Object>> tscNodes(nodeCount);
  v8::Local<v8::Value> ctorArgs[3];

  for (int i = 0; i < nodeCount; i++) {
    ctorArgs[0] = v8::Number::New(isolate, nodes[i].kind);
    ctorArgs[1] = v8::Number::New(isolate, nodes[i].start);
    ctorArgs[2] = v8::Number::New(isolate, nodes[i].end);
    auto maybeNode = NodeCtor->NewInstance(context, 3, ctorArgs);
    if (maybeNode.IsEmpty()) return;
    tscNodes[i] = maybeNode.ToLocalChecked();
    if (nodes[i].flags) tscNodes[i]->Set(context, s_flags, v8::Number::New(isolate, nodes[i].flags)).Check();
  }

  // Set parents
  for (int i = 0; i < nodeCount; i++) {
    if (nodes[i].parent != 0xFFFFFFFF && (int)nodes[i].parent < nodeCount) {
      tscNodes[i]->Set(context, s_parent, tscNodes[nodes[i].parent]).Check();
    }
  }

  // Set text on identifiers/string literals
  auto skVal = ts->Get(context, v8::String::NewFromUtf8Literal(isolate, "SyntaxKind")).ToLocalChecked().As<v8::Object>();
  int idKind = skVal->Get(context, v8::String::NewFromUtf8Literal(isolate, "Identifier")).ToLocalChecked()->Int32Value(context).FromMaybe(80);
  int strLitKind = skVal->Get(context, v8::String::NewFromUtf8Literal(isolate, "StringLiteral")).ToLocalChecked()->Int32Value(context).FromMaybe(11);
  int numLitKind = skVal->Get(context, v8::String::NewFromUtf8Literal(isolate, "NumericLiteral")).ToLocalChecked()->Int32Value(context).FromMaybe(9);
  auto s_text = v8::String::NewFromUtf8Literal(isolate, "text");
  auto s_escapedText = v8::String::NewFromUtf8Literal(isolate, "escapedText");

  for (int i = 0; i < nodeCount; i++) {
    int kind = nodes[i].kind;
    int s = nodes[i].start, e = nodes[i].end;
    if (kind == idKind) {
      while (s < e && ((*source)[s] <= ' ')) s++;
      while (e > s && ((*source)[e-1] <= ' ')) e--;
      if (e > s) {
        auto txt = v8::String::NewFromUtf8(isolate, *source + s, v8::NewStringType::kNormal, e - s).ToLocalChecked();
        tscNodes[i]->Set(context, s_escapedText, txt).Check();
        tscNodes[i]->Set(context, s_text, txt).Check();
      }
    } else if (kind == strLitKind && e > s + 1) {
      auto txt = v8::String::NewFromUtf8(isolate, *source + s + 1, v8::NewStringType::kNormal, e - s - 2).ToLocalChecked();
      tscNodes[i]->Set(context, s_text, txt).Check();
    } else if (kind == numLitKind) {
      while (s < e && (*source)[s] == ' ') s++;
      if (e > s) {
        auto txt = v8::String::NewFromUtf8(isolate, *source + s, v8::NewStringType::kNormal, e - s).ToLocalChecked();
        tscNodes[i]->Set(context, s_text, txt).Check();
      }
    }
  }

  // Call JS __zigMapChildren to map flat children → named properties
  auto mapFn = global->Get(context, v8::String::NewFromUtf8Literal(isolate, "__zigMapChildren")).ToLocalChecked();
  if (!mapFn->IsFunction()) {
    // Try globalThis
    mapFn = global->Get(context, v8::String::NewFromUtf8Literal(isolate, "__zigMapChildren")).ToLocalChecked();
  }
  // Create shared nodes array + flat buffer (reuse for both calls)
  auto nodesArr = v8::Array::New(isolate, nodeCount);
  for (int i = 0; i < nodeCount; i++) nodesArr->Set(context, i, tscNodes[i]).Check();
  size_t byte_len = (size_t)nodeCount * 24;
  auto ab = v8::ArrayBuffer::New(isolate, byte_len);
  auto backing = ab->GetBackingStore();
  if (backing && backing->Data()) {
    memcpy(backing->Data(), out_nodes, byte_len);
  }

  if (mapFn->IsFunction()) {
    v8::Local<v8::Value> mapArgs[2] = { nodesArr, ab };
    auto mapResult = mapFn.As<v8::Function>()->Call(context, global, 2, mapArgs);
    if (mapResult.IsEmpty()) return; // mapChildren failed
  }

  // Call JS __zigBuildSourceFile to assemble the SourceFile wrapper
  auto buildFn = global->Get(context, v8::String::NewFromUtf8Literal(isolate, "__zigBuildSourceFile")).ToLocalChecked();
  if (buildFn->IsFunction()) {
    v8::Local<v8::Value> buildArgs[4] = { nodesArr, args[0], args[1], ab };
    auto result = buildFn.As<v8::Function>()->Call(context, global, 4, buildArgs);
    if (!result.IsEmpty()) {
      args.GetReturnValue().Set(result.ToLocalChecked());
    }
  }
}

// ── C++ SourceFile creator (standalone, unused — Mode 2 of ZigParseCallback is preferred) ──
static void ZigCreateSourceFileCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  // JS: __edgebox_zig_create_sf(sourceText, fileName) → SourceFile object
  // Does: Zig parse → flat AST → create all V8 Node objects in C++ → return SourceFile
  if (args.Length() < 2) return;
  auto* isolate = args.GetIsolate();
  auto context = isolate->GetCurrentContext();

  v8::String::Utf8Value source(isolate, args[0]);
  v8::String::Utf8Value fileName(isolate, args[1]);
  if (!*source || !*fileName) return;

  // Step 1: Zig parse (fast — ~0.05ms per file)
  const char* out_nodes = nullptr;
  int out_count = 0;
  int ok = edgebox_zig_parse(*source, source.length(), &out_nodes, &out_count);
  if (ok != 1 || !out_nodes || out_count <= 0) return;

  const FlatNode* nodes = reinterpret_cast<const FlatNode*>(out_nodes);
  int nodeCount = out_count;

  // Step 2: Get the Node constructor from ts.objectAllocator
  // We need: globalThis.ts.objectAllocator.getNodeConstructor()
  auto global = context->Global();
  auto tsVal = global->Get(context, v8::String::NewFromUtf8Literal(isolate, "ts")).ToLocalChecked();
  if (!tsVal->IsObject()) return;
  auto ts = tsVal.As<v8::Object>();
  auto allocVal = ts->Get(context, v8::String::NewFromUtf8Literal(isolate, "objectAllocator")).ToLocalChecked();
  if (!allocVal->IsObject()) return;
  auto alloc = allocVal.As<v8::Object>();
  auto getNodeCtorVal = alloc->Get(context, v8::String::NewFromUtf8Literal(isolate, "getNodeConstructor")).ToLocalChecked();
  if (!getNodeCtorVal->IsFunction()) return;
  auto getNodeCtor = getNodeCtorVal.As<v8::Function>();
  auto nodeCtorVal = getNodeCtor->Call(context, alloc, 0, nullptr).ToLocalChecked();
  if (!nodeCtorVal->IsFunction()) return;
  auto NodeCtor = nodeCtorVal.As<v8::Function>();

  // Also get SourceFile constructor and SyntaxKind constants
  auto getSFCtorVal = alloc->Get(context, v8::String::NewFromUtf8Literal(isolate, "getSourceFileConstructor")).ToLocalChecked();
  v8::Local<v8::Function> SFCtor;
  if (getSFCtorVal->IsFunction()) {
    SFCtor = getSFCtorVal.As<v8::Function>()->Call(context, alloc, 0, nullptr).ToLocalChecked().As<v8::Function>();
  } else {
    SFCtor = NodeCtor; // fallback
  }

  auto skVal = ts->Get(context, v8::String::NewFromUtf8Literal(isolate, "SyntaxKind")).ToLocalChecked();
  if (!skVal->IsObject()) return;
  auto SK = skVal.As<v8::Object>();

  // Cache string keys for fast property access
  auto s_kind = v8::String::NewFromUtf8Literal(isolate, "kind");
  auto s_pos = v8::String::NewFromUtf8Literal(isolate, "pos");
  auto s_end = v8::String::NewFromUtf8Literal(isolate, "end");
  auto s_flags = v8::String::NewFromUtf8Literal(isolate, "flags");
  auto s_parent = v8::String::NewFromUtf8Literal(isolate, "parent");
  auto s_text = v8::String::NewFromUtf8Literal(isolate, "text");
  auto s_escapedText = v8::String::NewFromUtf8Literal(isolate, "escapedText");
  auto s_statements = v8::String::NewFromUtf8Literal(isolate, "statements");
  auto s_fileName = v8::String::NewFromUtf8Literal(isolate, "fileName");

  // Step 3: Create ALL V8 nodes in one pass
  std::vector<v8::Local<v8::Object>> tscNodes(nodeCount);
  v8::Local<v8::Value> ctorArgs[3];

  for (int i = 0; i < nodeCount; i++) {
    ctorArgs[0] = v8::Number::New(isolate, nodes[i].kind);
    ctorArgs[1] = v8::Number::New(isolate, nodes[i].start);
    ctorArgs[2] = v8::Number::New(isolate, nodes[i].end);
    auto maybeNode = NodeCtor->NewInstance(context, 3, ctorArgs);
    if (maybeNode.IsEmpty()) return;
    tscNodes[i] = maybeNode.ToLocalChecked();
    tscNodes[i]->Set(context, s_flags, v8::Number::New(isolate, nodes[i].flags)).Check();
  }

  // Step 4: Set parent references
  for (int i = 0; i < nodeCount; i++) {
    if (nodes[i].parent != 0xFFFFFFFF && (int)nodes[i].parent < nodeCount) {
      tscNodes[i]->Set(context, s_parent, tscNodes[nodes[i].parent]).Check();
    }
  }

  // Step 5: Set text on Identifiers and StringLiterals
  // Get SyntaxKind.Identifier and SyntaxKind.StringLiteral values
  auto idKindVal = SK->Get(context, v8::String::NewFromUtf8Literal(isolate, "Identifier")).ToLocalChecked();
  auto strLitKindVal = SK->Get(context, v8::String::NewFromUtf8Literal(isolate, "StringLiteral")).ToLocalChecked();
  auto numLitKindVal = SK->Get(context, v8::String::NewFromUtf8Literal(isolate, "NumericLiteral")).ToLocalChecked();
  int idKind = idKindVal->Int32Value(context).FromMaybe(80);
  int strLitKind = strLitKindVal->Int32Value(context).FromMaybe(11);
  int numLitKind = numLitKindVal->Int32Value(context).FromMaybe(9);

  for (int i = 0; i < nodeCount; i++) {
    int kind = nodes[i].kind;
    if (kind == idKind) {
      // Identifier: text = sourceText.substring(start, end).trim()
      int s = nodes[i].start, e = nodes[i].end;
      // Trim whitespace
      while (s < e && ((*source)[s] == ' ' || (*source)[s] == '\n' || (*source)[s] == '\r' || (*source)[s] == '\t')) s++;
      while (e > s && ((*source)[e-1] == ' ' || (*source)[e-1] == '\n' || (*source)[e-1] == '\r' || (*source)[e-1] == '\t')) e--;
      if (e > s) {
        auto txt = v8::String::NewFromUtf8(isolate, *source + s, v8::NewStringType::kNormal, e - s).ToLocalChecked();
        tscNodes[i]->Set(context, s_escapedText, txt).Check();
        tscNodes[i]->Set(context, s_text, txt).Check();
      }
    } else if (kind == strLitKind) {
      int s = nodes[i].start + 1, e = nodes[i].end - 1; // skip quotes
      if (e > s) {
        auto txt = v8::String::NewFromUtf8(isolate, *source + s, v8::NewStringType::kNormal, e - s).ToLocalChecked();
        tscNodes[i]->Set(context, s_text, txt).Check();
      }
    } else if (kind == numLitKind) {
      int s = nodes[i].start, e = nodes[i].end;
      while (s < e && (*source)[s] == ' ') s++;
      if (e > s) {
        auto txt = v8::String::NewFromUtf8(isolate, *source + s, v8::NewStringType::kNormal, e - s).ToLocalChecked();
        tscNodes[i]->Set(context, s_text, txt).Check();
      }
    }
  }

  // Step 6: Call JS-side mapChildren (reuse the version-proof bridge)
  // This is the only JS call — maps flat children to named properties
  auto mapChildrenFn = global->Get(context, v8::String::NewFromUtf8Literal(isolate, "__zigMapChildren")).ToLocalChecked();
  if (mapChildrenFn->IsFunction()) {
    // Pass the nodes array and flat data to JS for child mapping
    auto nodesArr = v8::Array::New(isolate, nodeCount);
    for (int i = 0; i < nodeCount; i++) {
      nodesArr->Set(context, i, tscNodes[i]).Check();
    }
    // Create ArrayBuffer view of flat nodes for child traversal
    size_t byte_len = (size_t)nodeCount * 24;
    auto ab = v8::ArrayBuffer::New(isolate, byte_len);
    memcpy(ab->GetBackingStore()->Data(), out_nodes, byte_len);

    v8::Local<v8::Value> mapArgs[2] = { nodesArr, ab };
    mapChildrenFn.As<v8::Function>()->Call(context, global, 2, mapArgs).ToLocalChecked();
  }

  // Step 7: Build SourceFile wrapper
  auto sfKindVal = SK->Get(context, v8::String::NewFromUtf8Literal(isolate, "SourceFile")).ToLocalChecked();
  int sfKind = sfKindVal->Int32Value(context).FromMaybe(308);
  ctorArgs[0] = v8::Number::New(isolate, sfKind);
  ctorArgs[1] = v8::Number::New(isolate, 0);
  ctorArgs[2] = v8::Number::New(isolate, source.length());
  auto sf = SFCtor->NewInstance(context, 3, ctorArgs).ToLocalChecked();

  sf->Set(context, s_flags, v8::Number::New(isolate, 0)).Check();
  sf->Set(context, s_text, args[0]).Check(); // original source text
  sf->Set(context, s_fileName, args[1]).Check();
  sf->Set(context, v8::String::NewFromUtf8Literal(isolate, "path"), v8::String::Empty(isolate)).Check();
  sf->Set(context, v8::String::NewFromUtf8Literal(isolate, "resolvedPath"), v8::String::Empty(isolate)).Check();
  sf->Set(context, v8::String::NewFromUtf8Literal(isolate, "originalFileName"), args[1]).Check();
  sf->Set(context, v8::String::NewFromUtf8Literal(isolate, "languageVersion"), v8::Number::New(isolate, 99)).Check();
  sf->Set(context, v8::String::NewFromUtf8Literal(isolate, "languageVariant"), v8::Number::New(isolate, 0)).Check();
  sf->Set(context, v8::String::NewFromUtf8Literal(isolate, "scriptKind"), v8::Number::New(isolate, 3)).Check();
  sf->Set(context, v8::String::NewFromUtf8Literal(isolate, "isDeclarationFile"), v8::Boolean::New(isolate, false)).Check();
  sf->Set(context, v8::String::NewFromUtf8Literal(isolate, "hasNoDefaultLib"), v8::Boolean::New(isolate, false)).Check();
  sf->Set(context, v8::String::NewFromUtf8Literal(isolate, "nodeCount"), v8::Number::New(isolate, nodeCount)).Check();
  sf->Set(context, v8::String::NewFromUtf8Literal(isolate, "identifierCount"), v8::Number::New(isolate, 0)).Check();
  sf->Set(context, v8::String::NewFromUtf8Literal(isolate, "symbolCount"), v8::Number::New(isolate, 0)).Check();

  // Empty arrays/maps
  auto emptyArr = v8::Array::New(isolate, 0);
  sf->Set(context, v8::String::NewFromUtf8Literal(isolate, "parseDiagnostics"), emptyArr).Check();
  sf->Set(context, v8::String::NewFromUtf8Literal(isolate, "bindDiagnostics"), emptyArr).Check();
  sf->Set(context, v8::String::NewFromUtf8Literal(isolate, "referencedFiles"), emptyArr).Check();
  sf->Set(context, v8::String::NewFromUtf8Literal(isolate, "typeReferenceDirectives"), emptyArr).Check();
  sf->Set(context, v8::String::NewFromUtf8Literal(isolate, "libReferenceDirectives"), emptyArr).Check();
  sf->Set(context, v8::String::NewFromUtf8Literal(isolate, "amdDependencies"), emptyArr).Check();
  sf->Set(context, v8::String::NewFromUtf8Literal(isolate, "imports"), emptyArr).Check();
  sf->Set(context, v8::String::NewFromUtf8Literal(isolate, "moduleAugmentations"), emptyArr).Check();
  sf->Set(context, v8::String::NewFromUtf8Literal(isolate, "ambientModuleNames"), emptyArr).Check();

  // Create Map objects via JS (can't create Map in C++ easily)
  auto mapCtorVal = global->Get(context, v8::String::NewFromUtf8Literal(isolate, "Map")).ToLocalChecked();
  if (mapCtorVal->IsFunction()) {
    auto MapCtor = mapCtorVal.As<v8::Function>();
    sf->Set(context, v8::String::NewFromUtf8Literal(isolate, "pragmas"), MapCtor->NewInstance(context, 0, nullptr).ToLocalChecked()).Check();
    sf->Set(context, v8::String::NewFromUtf8Literal(isolate, "identifiers"), MapCtor->NewInstance(context, 0, nullptr).ToLocalChecked()).Check();
  }

  // Root children = statements
  uint32_t rootFirst = nodes[0].first_child;
  auto stmts = v8::Array::New(isolate);
  int stmtIdx = 0;
  uint32_t ch = rootFirst;
  while (ch != 0xFFFFFFFF && (int)ch < nodeCount) {
    tscNodes[ch]->Set(context, s_parent, sf).Check();
    stmts->Set(context, stmtIdx++, tscNodes[ch]).Check();
    ch = nodes[ch].next_sibling;
  }
  stmts->Set(context, v8::String::NewFromUtf8Literal(isolate, "pos"), v8::Number::New(isolate, -1)).Check();
  stmts->Set(context, v8::String::NewFromUtf8Literal(isolate, "end"), v8::Number::New(isolate, -1)).Check();
  stmts->Set(context, v8::String::NewFromUtf8Literal(isolate, "hasTrailingComma"), v8::Boolean::New(isolate, false)).Check();
  stmts->Set(context, v8::String::NewFromUtf8Literal(isolate, "transformFlags"), v8::Number::New(isolate, 0)).Check();
  sf->Set(context, s_statements, stmts).Check();

  // EndOfFileToken
  auto eofKindVal = SK->Get(context, v8::String::NewFromUtf8Literal(isolate, "EndOfFileToken")).ToLocalChecked();
  int eofKind = eofKindVal->Int32Value(context).FromMaybe(1);
  auto getTokenCtorVal = alloc->Get(context, v8::String::NewFromUtf8Literal(isolate, "getTokenConstructor")).ToLocalChecked();
  if (getTokenCtorVal->IsFunction()) {
    ctorArgs[0] = v8::Number::New(isolate, eofKind);
    ctorArgs[1] = v8::Number::New(isolate, source.length());
    ctorArgs[2] = v8::Number::New(isolate, source.length());
    auto eof = getTokenCtorVal.As<v8::Function>()->Call(context, alloc, 0, nullptr).ToLocalChecked().As<v8::Function>()->NewInstance(context, 3, ctorArgs).ToLocalChecked();
    eof->Set(context, s_parent, sf).Check();
    sf->Set(context, v8::String::NewFromUtf8Literal(isolate, "endOfFileToken"), eof).Check();
  }

  args.GetReturnValue().Set(sf);
}

// Setup context with IO globals. If snapshot exists, uses snapshot context.
// Otherwise creates a new context.
void* edgebox_v8_setup_context(void* iso_ptr) {
  auto* isolate = static_cast<v8::Isolate*>(iso_ptr);
  v8::Isolate::Scope isolate_scope(isolate);
  v8::HandleScope handle_scope(isolate);

  v8::Local<v8::Context> context;

  if (g_snapshot.data) {
    // Restore snapshot context (TypeScript already loaded)
    context = v8::Context::New(isolate);
  } else {
    // Fresh context with IO globals template
    auto global = v8::ObjectTemplate::New(isolate);
    global->Set(isolate, "__edgebox_read_file", v8::FunctionTemplate::New(isolate, ReadFileCallback));
    global->Set(isolate, "__edgebox_read_binary", v8::FunctionTemplate::New(isolate, ReadBinaryCallback));
    global->Set(isolate, "__edgebox_file_exists", v8::FunctionTemplate::New(isolate, FileExistsCallback));
    global->Set(isolate, "__edgebox_dir_exists", v8::FunctionTemplate::New(isolate, DirExistsCallback));
    global->Set(isolate, "__edgebox_stat", v8::FunctionTemplate::New(isolate, StatCallback));
    global->Set(isolate, "__edgebox_readdir", v8::FunctionTemplate::New(isolate, ReaddirCallback));
    global->Set(isolate, "__edgebox_realpath", v8::FunctionTemplate::New(isolate, RealpathCallback));
    global->Set(isolate, "__edgebox_cwd", v8::FunctionTemplate::New(isolate, CwdCallback));
    global->Set(isolate, "__edgebox_write_stdout", v8::FunctionTemplate::New(isolate, WriteStdoutCallback));
    global->Set(isolate, "__edgebox_write_stderr", v8::FunctionTemplate::New(isolate, WriteStderrCallback));
    global->Set(isolate, "__edgebox_hash", v8::FunctionTemplate::New(isolate, HashCallback));
    global->Set(isolate, "__edgebox_exit", v8::FunctionTemplate::New(isolate, ExitCallback));
    context = v8::Context::New(isolate, nullptr, global);
  }

  v8::Context::Scope context_scope(context);

  if (!g_snapshot.data) {
    // No snapshot — register IO globals on the fresh context
    auto globalObj = context->Global();
    auto set = [&](const char* name, v8::FunctionCallback cb) {
      globalObj->Set(context,
        v8::String::NewFromUtf8(isolate, name).ToLocalChecked(),
        v8::Function::New(context, cb).ToLocalChecked()).Check();
    };
    set("__edgebox_read_file", ReadFileCallback);
    set("__edgebox_read_binary", ReadBinaryCallback);
    set("__edgebox_file_exists", FileExistsCallback);
    set("__edgebox_dir_exists", DirExistsCallback);
    set("__edgebox_stat", StatCallback);
    set("__edgebox_readdir", ReaddirCallback);
    set("__edgebox_realpath", RealpathCallback);
    set("__edgebox_cwd", CwdCallback);
    set("__edgebox_write_stdout", WriteStdoutCallback);
    set("__edgebox_write_stderr", WriteStderrCallback);
    set("__edgebox_hash", HashCallback);
    set("__edgebox_exit", ExitCallback);
    set("__edgebox_submit_result", SubmitResultCallback);
    // Type system callbacks removed — all in WasmGC now
    set("__edgebox_claim_file", ClaimFileCallback);
    set("__edgebox_signal_program_ready", SignalProgramReadyCallback);
    set("__edgebox_wait_program_ready", WaitProgramReadyCallback);
    set("__edgebox_worker_done", WorkerDoneCallback);
  }
  // SF store callbacks — register on ALL contexts (snapshot may not preserve template functions)
  {
    auto globalObj = context->Global();
    auto set = [&](const char* name, v8::FunctionCallback cb) {
      globalObj->Set(context,
        v8::String::NewFromUtf8(isolate, name).ToLocalChecked(),
        v8::Function::New(context, cb).ToLocalChecked()).Check();
    };
    set("__edgebox_sf_store_put", SfStorePutCallback);
    set("__edgebox_sf_store_get", SfStoreGetCallback);
    set("__edgebox_sf_store_count", SfStoreCountCallback);
    set("__edgebox_zig_create_sf", ZigCreateSourceFileCallback);
  }

  auto* persistent = new v8::Persistent<v8::Context>(isolate, context);
  return persistent;
}

// Eval JS in a persistent context
const char* edgebox_v8_eval_in_context(void* iso_ptr, void* ctx_ptr, const char* code, int code_len, int* out_len) {
  auto* isolate = static_cast<v8::Isolate*>(iso_ptr);
  auto* persistent = static_cast<v8::Persistent<v8::Context>*>(ctx_ptr);

  v8::Isolate::Scope isolate_scope(isolate);
  v8::HandleScope handle_scope(isolate);
  auto context = persistent->Get(isolate);
  v8::Context::Scope context_scope(context);

  auto source = v8::String::NewFromUtf8(isolate, code, v8::NewStringType::kNormal, code_len);
  if (source.IsEmpty()) { *out_len = 0; return nullptr; }

  auto script = v8::Script::Compile(context, source.ToLocalChecked());
  if (script.IsEmpty()) { *out_len = 0; return nullptr; }

  v8::TryCatch try_catch(isolate);
  auto result = script.ToLocalChecked()->Run(context);
  if (result.IsEmpty()) {
    if (try_catch.HasCaught()) {
      v8::String::Utf8Value err(isolate, try_catch.Exception());
      if (*err) {
        int len = err.length();
        char* copy = new char[len + 1];
        memcpy(copy, *err, len);
        copy[len] = '\0';
        *out_len = len;
        return copy;
      }
    }
    *out_len = 0;
    return nullptr;
  }

  v8::String::Utf8Value utf8(isolate, result.ToLocalChecked());
  if (*utf8 == nullptr) { *out_len = 0; return nullptr; }

  int len = utf8.length();
  char* copy = new char[len + 1];
  memcpy(copy, *utf8, len);
  copy[len] = '\0';
  *out_len = len;
  return copy;
}

// Store TSC source for per-worker code-cache compilation.
void edgebox_v8_store_ts(const char* ts_code, int ts_len) {
  char* ts_copy = new char[ts_len];
  memcpy(ts_copy, ts_code, ts_len);
  g_ts_code = ts_copy;
  g_ts_code_len = ts_len;
}

// Compile TSC from code cache in a worker context.
// Code cache contains TurboFan-compiled code from build-time warmup.
// Workers get instant TurboFan — zero compilation cost.
const char* edgebox_v8_compile_ts_cached(void* iso_ptr, void* ctx_ptr, int* out_len) {
  if (!g_ts_code || g_ts_code_len == 0 || !g_code_cache_data || g_code_cache_len == 0) {
    *out_len = 0;
    return nullptr; // No code cache available
  }

  auto* isolate = static_cast<v8::Isolate*>(iso_ptr);
  auto* persistent = static_cast<v8::Persistent<v8::Context>*>(ctx_ptr);

  v8::Isolate::Scope isolate_scope(isolate);
  v8::HandleScope handle_scope(isolate);
  auto context = persistent->Get(isolate);
  v8::Context::Scope context_scope(context);

  auto source_str = v8::String::NewFromUtf8(isolate, g_ts_code,
      v8::NewStringType::kNormal, g_ts_code_len);
  if (source_str.IsEmpty()) { *out_len = 0; return nullptr; }

  // Create CachedData from our stored code cache.
  // BufferNotOwned = we manage the memory, V8 just reads it.
  auto* cached = new v8::ScriptCompiler::CachedData(
      g_code_cache_data, g_code_cache_len,
      v8::ScriptCompiler::CachedData::BufferNotOwned);

  v8::ScriptOrigin origin(v8::String::NewFromUtf8Literal(isolate, "typescript.js"));
  v8::ScriptCompiler::Source source(source_str.ToLocalChecked(), origin, cached);

  v8::TryCatch try_catch(isolate);
  auto maybe_script = v8::ScriptCompiler::Compile(context, &source,
      v8::ScriptCompiler::kConsumeCodeCache);

  if (maybe_script.IsEmpty()) {
    if (try_catch.HasCaught()) {
      v8::String::Utf8Value err(isolate, try_catch.Exception());
      if (*err) {
        int len = err.length();
        char* copy = new char[len + 1];
        memcpy(copy, *err, len);
        copy[len] = '\0';
        *out_len = len;
        return copy;
      }
    }
    *out_len = 0;
    return nullptr;
  }

  bool rejected = cached->rejected;
  // Don't Run — TSC is already loaded from snapshot.
  // The Compile + kConsumeCodeCache deserialized TurboFan code from the cache.
  // V8's in-memory compilation cache now has TurboFan code for all warmed functions.
  (void)maybe_script.ToLocalChecked();

  char buf[128];
  int n = snprintf(buf, sizeof(buf), "[v8pool] code cache: loaded %d bytes (rejected=%d)\n",
      g_code_cache_len, rejected ? 1 : 0);
  edgebox_write_stderr(buf, n);

  *out_len = 0;
  return nullptr;
}

// Check if code cache is available
int edgebox_v8_has_code_cache() {
  return (g_code_cache_data && g_code_cache_len > 0) ? 1 : 0;
}

} // extern "C"
