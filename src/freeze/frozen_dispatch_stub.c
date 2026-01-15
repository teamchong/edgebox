// Stub implementation of frozen_dispatch_lookup for CLI builds
// The CLI (edgeboxc) compiles code but doesn't execute frozen functions
// This provides a no-op implementation that always returns 0 (no frozen function found)

#include <stddef.h>
#include <stdint.h>

// Forward declaration of JSContext and JSValue (opaque types)
typedef struct JSContext JSContext;
typedef uint64_t JSValue;

// Stub: always return 0 (no frozen function found)
int frozen_dispatch_lookup(JSContext *ctx, const char *func_name,
                           JSValue this_val, int argc, JSValue *argv,
                           JSValue *result_out) {
    (void)ctx;
    (void)func_name;
    (void)this_val;
    (void)argc;
    (void)argv;
    (void)result_out;
    return 0;  // No frozen function found
}
