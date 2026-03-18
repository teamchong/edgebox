// Frozen dispatch for CLI builds
// The CLI (edgebox) compiles code but doesn't execute frozen functions
// This provides a no-op implementation - the real dispatch is in native_dispatch.zig

#include <stddef.h>
#include <stdint.h>

// Compressed heap base - also defined in profile_counters.c for native binary.
// CLI doesn't use it but needs the symbols for linking.
size_t edgebox_compressed_heap_base = 0;

void edgebox_set_compressed_heap_base(size_t base) {
    edgebox_compressed_heap_base = base;
}

// Check if frozen dispatch is enabled - always false in CLI mode
int frozen_dispatch_is_enabled(void) {
    return 0;
}

// Get frozen function pointer by parser index
// Returns NULL - real dispatch is in Zig runtime
void *frozen_dispatch_get_by_index(int idx) {
    (void)idx;
    return NULL;
}

// Native Shapes fast path for AST property access
// Returns 0 (not found) - CLI compiles but doesn't execute frozen native shapes
int native_shapes_get_property(uint64_t obj_addr, uint32_t atom, int32_t *value_out) {
    (void)obj_addr;
    (void)atom;
    (void)value_out;
    return 0;  // Not found in CLI mode
}

// Initialize atom cache for native shapes fast path
// No-op in CLI mode - we don't run the compiled code
void native_shapes_init_atoms(uint32_t atom_kind, uint32_t atom_flags, uint32_t atom_pos, uint32_t atom_end) {
    (void)atom_kind;
    (void)atom_flags;
    (void)atom_pos;
    (void)atom_end;
}

// Debug stats - no-op in CLI mode
void native_shapes_debug_stats(void) {
}

// Call profiling stubs - weak symbols overridden by Zig exports in CLI builds
__attribute__((weak)) int edgebox_call_profile_is_enabled = 0;

__attribute__((weak)) void edgebox_call_profile_increment(uint32_t atom, uint32_t line_num) {
    (void)atom;
    (void)line_num;
}
