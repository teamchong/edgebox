// v8_bridge.cpp — Minimal C++ bridge for V8 operations requiring struct layouts.
// Zig calls these helpers for operations that need C++ struct knowledge
// (CreateParams allocator setup, ScriptOrigin construction, etc.).
// Everything else is called directly from Zig via binding.cc extern "C" functions.

#include <cstddef>
#include <cstdint>
#include <cstring>

#include "v8-isolate.h"
#include "v8-script.h"

// Functions from librusty_v8.a (binding.cc)
extern "C" {
extern v8::ArrayBuffer::Allocator*
v8__ArrayBuffer__Allocator__NewDefaultAllocator();
}

extern "C" {

// --- Isolate Creation ---
// Creates an Isolate with a default ArrayBuffer allocator.
// This avoids Zig needing to know CreateParams struct layout.
v8::Isolate* edgebox_v8_create_isolate() {
  v8::Isolate::CreateParams params;
  params.array_buffer_allocator =
      v8__ArrayBuffer__Allocator__NewDefaultAllocator();
  return v8::Isolate::New(params);
}

// --- Struct Sizes ---
// Returns sizeof(ScriptOrigin) so Zig can allocate the right buffer
// for v8__ScriptOrigin__CONSTRUCT.
size_t edgebox_v8_script_origin_sizeof() {
  return sizeof(v8::ScriptOrigin);
}

// Returns sizeof(CreateParams) — same as v8__Isolate__CreateParams__SIZEOF()
// but available without calling into librusty_v8.
size_t edgebox_v8_create_params_sizeof() {
  return sizeof(v8::Isolate::CreateParams);
}

} // extern "C"
