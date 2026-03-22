// V8 Bridge for Pool — uses V8 headers for correct struct layout

#include <v8-isolate.h>
#include <v8-context.h>
#include <v8-script.h>
#include <v8-primitive.h>
#include <v8-local-handle.h>
#include <v8-value.h>
#include <cstring>

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

} // extern "C"
