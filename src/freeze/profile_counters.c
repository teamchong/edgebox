/**
 * Profile counter storage for frozen interpreter.
 * 
 * This file provides the single definition of the profile counter array
 * and the increment function. All frozen shards reference these symbols
 * via extern linkage, ensuring they all share the same counters.
 */

#include <stdint.h>

/* Counter array - indices must match profile.zig */
uint64_t g_frozen_profile_counters[10] = {0};

/* Noinline increment to prevent optimizer elision in shard objects */
__attribute__((noinline))
void frozen_profile_vinc(uint64_t *ptr, uint64_t delta) {
    *ptr += delta;
}
