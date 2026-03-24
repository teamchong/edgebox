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
#include <cstring>
#include <cstdio>
#include <string>
#include <unistd.h>

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
  const char* default_flags = "--max-old-space-size=4096 --concurrent-sparkplug --max-semi-space-size=16 --turbo-inline-js-wasm-calls --allow-natives-syntax";
  char flags_buf[1024];
  if (extra_flags && strlen(extra_flags) > 0) {
    snprintf(flags_buf, sizeof(flags_buf), "%s %s", default_flags, extra_flags);
  } else {
    snprintf(flags_buf, sizeof(flags_buf), "%s", default_flags);
  }
  const char* flags = flags_buf;
  v8__V8__SetFlagsFromString(flags, strlen(flags));
  // 8 platform threads for background JIT compilation across 8 V8 isolates
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
// Dead stubs — type system migrated to WasmGC. Kept for external_refs compatibility.
static void NoopCallback(const v8::FunctionCallbackInfo<v8::Value>&) {}

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
    // IOStats removed — migrated to WasmGC
    global->Set(isolate, "__edgebox_root", v8::FunctionTemplate::New(isolate, RootCallback));
    global->Set(isolate, "__edgebox_resolve_cache_get", v8::FunctionTemplate::New(isolate, ResolveCacheGetCallback));
    global->Set(isolate, "__edgebox_resolve_cache_set", v8::FunctionTemplate::New(isolate, ResolveCacheSetCallback));
    global->Set(isolate, "__edgebox_write_file", v8::FunctionTemplate::New(isolate, WriteFileCallback));
    // IsSimpleTypeRelated removed — migrated to WasmGC
    global->Set(isolate, "__edgebox_submit_result", v8::FunctionTemplate::New(isolate, SubmitResultCallback));
    global->Set(isolate, "__edgebox_worker_done", v8::FunctionTemplate::New(isolate, WorkerDoneCallback));
    auto context = v8::Context::New(isolate, nullptr, global);
    v8::Context::Scope context_scope(context);

    // 1. Eval shim (module, process stubs, require stubs)
    EvalInContext(isolate, context, shim_code, shim_len);
    // 2. Eval TypeScript (compiled + executed)
    EvalInContext(isolate, context, ts_code, ts_len);
    // 3. Set ts alias
    EvalInContext(isolate, context, "globalThis.ts = globalThis.module.exports;", 42);
    // 4. Eval worker_init (process, require with real IO)
    EvalInContext(isolate, context, init_code, init_len);
    // 5. Eval recipe (__edgebox_check defined)
    EvalInContext(isolate, context, recipe_code, recipe_len);

    creator.SetDefaultContext(context);
  }

  g_snapshot = creator.CreateBlob(v8::SnapshotCreator::FunctionCodeHandling::kKeep);
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
extern void edgebox_write_stderr(const char* data, int len);
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
  // Snapshot workers: IO callbacks already baked in via external_references

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

} // extern "C"
