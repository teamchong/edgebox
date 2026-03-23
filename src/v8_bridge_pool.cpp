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
#include <cstring>
#include <string>

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
  const char* flags = "--max-old-space-size=4096";
  v8__V8__SetFlagsFromString(flags, strlen(flags));
  g_platform = v8__Platform__NewDefaultPlatform(0, false);
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

// ── Snapshot: pre-compile TypeScript for instant worker startup ──

static v8::StartupData g_snapshot = {nullptr, 0};

// Create a snapshot with TypeScript pre-loaded
int edgebox_v8_create_snapshot(const char* ts_code, int ts_len, const char* shim_code, int shim_len) {
  v8::SnapshotCreator creator;
  auto* isolate = creator.GetIsolate();
  {
    v8::Isolate::Scope isolate_scope(isolate);
    v8::HandleScope handle_scope(isolate);
    auto context = v8::Context::New(isolate);
    v8::Context::Scope context_scope(context);

    // Eval shim (module, process, require, Buffer, setTimeout)
    if (shim_code && shim_len > 0) {
      auto source = v8::String::NewFromUtf8(isolate, shim_code, v8::NewStringType::kNormal, shim_len);
      if (!source.IsEmpty()) {
        auto script = v8::Script::Compile(context, source.ToLocalChecked());
        if (!script.IsEmpty()) script.ToLocalChecked()->Run(context);
      }
    }

    // Eval TypeScript
    if (ts_code && ts_len > 0) {
      auto source = v8::String::NewFromUtf8(isolate, ts_code, v8::NewStringType::kNormal, ts_len);
      if (!source.IsEmpty()) {
        auto script = v8::Script::Compile(context, source.ToLocalChecked());
        if (!script.IsEmpty()) script.ToLocalChecked()->Run(context);
      }
    }

    // Set ts = module.exports
    {
      auto source = v8::String::NewFromUtf8Literal(isolate, "globalThis.ts = globalThis.module.exports;");
      auto script = v8::Script::Compile(context, source);
      if (!script.IsEmpty()) script.ToLocalChecked()->Run(context);
    }

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
  return v8::Isolate::New(params);
}

// Register IO globals on a context — connects V8 to Zig polyfills
// These are the same C ABI functions from edgebox_workerd_io.zig
extern const char* edgebox_read_file(const char* path, int path_len, int* out_len);
extern int edgebox_file_exists(const char* path, int path_len);
extern int edgebox_dir_exists(const char* path, int path_len);
extern const char* edgebox_stat(const char* path, int path_len, int* out_len);
extern const char* edgebox_readdir(const char* path, int path_len, int* out_len);
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
  auto path = GetStringArg(args, 0);
  int out_len = 0;
  auto* data = edgebox_read_file(path.c_str(), path.size(), &out_len);
  if (!data || out_len <= 0) {
    args.GetReturnValue().Set(v8::String::NewFromUtf8(args.GetIsolate(), "").ToLocalChecked());
    return;
  }
  args.GetReturnValue().Set(
    v8::String::NewFromUtf8(args.GetIsolate(), data, v8::NewStringType::kNormal, out_len).ToLocalChecked());
}

static void FileExistsCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  auto path = GetStringArg(args, 0);
  args.GetReturnValue().Set(v8::Number::New(args.GetIsolate(), edgebox_file_exists(path.c_str(), path.size())));
}

static void DirExistsCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  auto path = GetStringArg(args, 0);
  args.GetReturnValue().Set(v8::Number::New(args.GetIsolate(), edgebox_dir_exists(path.c_str(), path.size())));
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
  auto result = v8::String::NewFromUtf8(iso, data, v8::NewStringType::kNormal, out_len).ToLocalChecked();
  edgebox_free(data, out_len);
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

extern void edgebox_submit_result(int worker_id, const char* data, int data_len);
extern void edgebox_worker_done(int worker_id);

static void SubmitResultCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  if (args.Length() < 2) return;
  int wid = args[0]->Int32Value(args.GetIsolate()->GetCurrentContext()).FromMaybe(0);
  auto str = GetStringArg(args, 1);
  // Debug: check for 0x0A vs 0x5C+0x6E
  if (wid == 0 && str.size() > 0) {
    int real_nl = 0, lit_nl = 0;
    for (size_t i = 0; i < str.size(); i++) {
      if (str[i] == '\n') real_nl++;
      if (i + 1 < str.size() && str[i] == '\\' && str[i+1] == 'n') lit_nl++;
    }
    char dbg[128];
    snprintf(dbg, sizeof(dbg), "[cpp] submit wid=%d len=%zu real_nl=%d lit_nl=%d\n", wid, str.size(), real_nl, lit_nl);
    write(2, dbg, strlen(dbg));
  }
  edgebox_submit_result(wid, str.c_str(), str.size());
}

static void WorkerDoneCallback(const v8::FunctionCallbackInfo<v8::Value>& args) {
  if (args.Length() < 1) return;
  int wid = args[0]->Int32Value(args.GetIsolate()->GetCurrentContext()).FromMaybe(0);
  edgebox_worker_done(wid);
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

  // Enter context and register IO globals (needed for both snapshot and fresh)
  v8::Context::Scope context_scope(context);
  auto globalObj = context->Global();

  auto set = [&](const char* name, v8::FunctionCallback cb) {
    globalObj->Set(context,
      v8::String::NewFromUtf8(isolate, name).ToLocalChecked(),
      v8::Function::New(context, cb).ToLocalChecked()).Check();
  };

  set("__edgebox_read_file", ReadFileCallback);
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
  set("__edgebox_worker_done", WorkerDoneCallback);

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
