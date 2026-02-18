/**
 * Profile counter storage for frozen interpreter.
 * 
 * This file provides the single definition of the profile counter array
 * and the increment function. All frozen shards reference these symbols
 * via extern linkage, ensuring they all share the same counters.
 */

#include <stddef.h>
#include <stdint.h>

/* Counter array - indices must match profile.zig */
uint64_t g_frozen_profile_counters[10] = {0};

/**
 * Compressed heap base for pointer compression on native 64-bit.
 * All frozen Zig shards call edgebox_get/set_compressed_heap_base() to access this.
 * Without a single C definition, each Zig compilation unit gets its own copy
 * initialized to 0, and LLVM constant-folds it, breaking Linux x86_64 pointers.
 */
/* NOT static — Zig shards reference this directly via extern var */
size_t edgebox_compressed_heap_base = 0;

void edgebox_set_compressed_heap_base(size_t base) {
    edgebox_compressed_heap_base = base;
}

/* Noinline increment to prevent optimizer elision in shard objects */
__attribute__((noinline))
void frozen_profile_vinc(uint64_t *ptr, uint64_t delta) {
    *ptr += delta;
}
