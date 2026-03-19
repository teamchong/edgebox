// v8_bridge.cpp — Minimal C++ bridge for V8 operations requiring struct layouts.
// Zig calls these helpers for operations that need C++ struct knowledge
// (CreateParams allocator setup, ScriptOrigin construction, etc.).
// Everything else is called directly from Zig via binding.cc extern "C" functions.

#include <cstddef>
#include <cstdint>
#include <cstring>

#include "v8-isolate.h"
#include "v8-script.h"
#include "v8-context.h"
#include "v8-snapshot.h"

// Functions from librusty_v8.a (binding.cc)
extern "C" {
extern v8::ArrayBuffer::Allocator*
v8__ArrayBuffer__Allocator__NewDefaultAllocator();

extern void v8__SnapshotCreator__CONSTRUCT(
    void* buf, const v8::Isolate::CreateParams& params);
extern void v8__SnapshotCreator__DESTRUCT(v8::SnapshotCreator* self);
extern v8::Isolate* v8__SnapshotCreator__GetIsolate(
    const v8::SnapshotCreator& self);
extern void v8__SnapshotCreator__SetDefaultContext(
    v8::SnapshotCreator* self, const v8::Context& context);
extern v8::StartupData v8__SnapshotCreator__CreateBlob(
    v8::SnapshotCreator* self,
    v8::SnapshotCreator::FunctionCodeHandling function_code_handling);
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

// Creates an Isolate from a V8 snapshot blob with external references.
// The snapshot data must remain valid for the lifetime of the isolate.
// external_refs is a null-terminated array of function pointers — must match
// the array used when creating the snapshot.
// Uses static storage for StartupData to ensure it outlives Isolate::New.
v8::Isolate* edgebox_v8_create_isolate_from_snapshot(
    const char* snapshot_data, int snapshot_len,
    const intptr_t* external_refs) {
  // Static — V8 may reference this after Isolate::New returns
  static v8::StartupData startup;
  startup.data = snapshot_data;
  startup.raw_size = snapshot_len;

  v8::Isolate::CreateParams params;
  params.array_buffer_allocator =
      v8__ArrayBuffer__Allocator__NewDefaultAllocator();
  params.snapshot_blob = &startup;
  if (external_refs) {
    params.external_references = external_refs;
  }
  return v8::Isolate::New(params);
}

// Creates a SnapshotCreator with external references.
// The buf must be at least sizeof(SnapshotCreator) = sizeof(size_t) bytes.
// external_refs is a null-terminated array of function pointers.
void edgebox_v8_snapshot_creator_new(
    void* buf,
    const intptr_t* external_refs) {
  v8::Isolate::CreateParams params;
  params.array_buffer_allocator =
      v8__ArrayBuffer__Allocator__NewDefaultAllocator();
  if (external_refs) {
    params.external_references = external_refs;
  }
  v8__SnapshotCreator__CONSTRUCT(buf, params);
}

// --- Struct Sizes ---
size_t edgebox_v8_script_origin_sizeof() {
  return sizeof(v8::ScriptOrigin);
}

size_t edgebox_v8_create_params_sizeof() {
  return sizeof(v8::Isolate::CreateParams);
}

size_t edgebox_v8_source_sizeof() {
  return sizeof(v8::ScriptCompiler::Source);
}

size_t edgebox_v8_snapshot_creator_sizeof() {
  return sizeof(v8::SnapshotCreator);
}

} // extern "C"
