// Frozen dispatch for CLI builds
// The CLI (edgeboxc) compiles code but doesn't execute frozen functions
// This provides a no-op implementation - the real dispatch is in native_dispatch.zig

#include <stddef.h>
#include <stdint.h>

// Forward declaration of JSContext and JSValue (opaque types)
typedef struct JSContext JSContext;
typedef uint64_t JSValue;
typedef struct JSVarRef JSVarRef;

// Name-based dispatch (with var_refs, cpool, and bytecode_ptr for closure/fclosure support)
// Returns 0 (no frozen function found) - real dispatch is in Zig runtime
int frozen_dispatch_lookup(JSContext *ctx, const char *func_name,
                           JSValue this_val, int argc, JSValue *argv,
                           JSVarRef **var_refs, JSValue *cpool,
                           void *bytecode_ptr, JSValue *result_out) {
    (void)ctx;
    (void)func_name;
    (void)this_val;
    (void)argc;
    (void)argv;
    (void)var_refs;
    (void)cpool;
    (void)bytecode_ptr;
    (void)result_out;
    return 0;  // No frozen function found in CLI mode
}

// Bytecode-based dispatch (for closure/fclosure support)
// Returns 0 (no frozen function found) - real dispatch is in Zig runtime
int frozen_dispatch_lookup_bytecode(JSContext *ctx, void *bytecode_ptr,
                                    JSValue this_val, int argc, JSValue *argv,
                                    JSVarRef **var_refs, int closure_var_count,
                                    JSValue *cpool,
                                    JSValue *result_out) {
    (void)ctx;
    (void)bytecode_ptr;
    (void)this_val;
    (void)argc;
    (void)argv;
    (void)var_refs;
    (void)closure_var_count;
    (void)cpool;
    (void)result_out;
    return 0;  // No frozen function found in CLI mode
}
