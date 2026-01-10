/*
 * Frozen Runtime - Shared helpers for all frozen functions
 * Pre-compiled once, linked into all WASM builds
 *
 * Contains:
 * - SMI (Small Integer) arithmetic helpers
 * - Comparison helpers
 * - Bitwise operation helpers
 * - Stack management macros (in header)
 */

#include "quickjs.h"
#include <stdint.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#ifndef likely
#define likely(x) __builtin_expect(!!(x), 1)
#endif
#ifndef unlikely
#define unlikely(x) __builtin_expect(!!(x), 0)
#endif

/* Call stack limit - matches Node.js default */
#ifndef FROZEN_MAX_CALL_DEPTH
#define FROZEN_MAX_CALL_DEPTH 10000
#endif

/* Global call depth counter - reset between requests */
int frozen_call_depth = 0;

/* Re-entry flag to prevent infinite loop in partial freeze fallback */
/* When set, hook injection should skip redirecting to frozen version */
int frozen_fallback_active = 0;

/* Reset call depth (call at start of each request) */
void frozen_reset_call_depth(void) {
    frozen_call_depth = 0;
}

/* ============================================================================
 * Safe Call Helper - just wraps JS_Call
 * If the value isn't callable, JS_Call will throw the appropriate error.
 * ============================================================================ */
JSValue frozen_safe_call(JSContext *ctx, JSValue func, JSValue this_obj, int argc, JSValue *argv) {
    return JS_Call(ctx, func, this_obj, argc, argv);
}

/* ============================================================================
 * SMI (Small Integer) Arithmetic Helpers
 * Zero allocation fast path for int32 operands
 * Falls back to float64 only on overflow or non-int input
 * ============================================================================ */

// Helper to concatenate two strings (since JS_ConcatString is not exported)
static JSValue frozen_string_concat(JSContext *ctx, JSValue str_a, JSValue str_b) {
    const char *cstr_a = JS_ToCString(ctx, str_a);
    const char *cstr_b = JS_ToCString(ctx, str_b);
    if (!cstr_a || !cstr_b) {
        if (cstr_a) JS_FreeCString(ctx, cstr_a);
        if (cstr_b) JS_FreeCString(ctx, cstr_b);
        return JS_EXCEPTION;
    }
    size_t len_a = strlen(cstr_a);
    size_t len_b = strlen(cstr_b);
    char *buf = js_malloc(ctx, len_a + len_b + 1);
    if (!buf) {
        JS_FreeCString(ctx, cstr_a);
        JS_FreeCString(ctx, cstr_b);
        return JS_EXCEPTION;
    }
    memcpy(buf, cstr_a, len_a);
    memcpy(buf + len_a, cstr_b, len_b + 1);
    JS_FreeCString(ctx, cstr_a);
    JS_FreeCString(ctx, cstr_b);
    JSValue result = JS_NewString(ctx, buf);
    js_free(ctx, buf);
    return result;
}

/* Wrapper for inline JS_NewString function (for Zig FFI) */
JSValue frozen_new_string(JSContext *ctx, const char *str) {
    return JS_NewString(ctx, str);
}

/* Wrapper for inline JS_NewCFunction function (for Zig FFI) */
JSValue frozen_new_cfunction(JSContext *ctx, JSCFunction *func, const char *name, int length) {
    return JS_NewCFunction(ctx, func, name, length);
}

JSValue frozen_add(JSContext *ctx, JSValue a, JSValue b) {
    // Fast path: int + int
    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
        int64_t r = (int64_t)JS_VALUE_GET_INT(a) + JS_VALUE_GET_INT(b);
        if (likely((int32_t)r == r)) return JS_MKVAL(JS_TAG_INT, (int32_t)r);
        return JS_NewFloat64(ctx, (double)r);
    }
    // String concatenation path - convert both to string if either is a string
    if (JS_VALUE_GET_TAG(a) == JS_TAG_STRING || JS_VALUE_GET_TAG(b) == JS_TAG_STRING) {
        JSValue str_a = JS_ToString(ctx, a);
        if (JS_IsException(str_a)) return str_a;
        JSValue str_b = JS_ToString(ctx, b);
        if (JS_IsException(str_b)) {
            JS_FreeValue(ctx, str_a);
            return str_b;
        }
        JSValue result = frozen_string_concat(ctx, str_a, str_b);
        JS_FreeValue(ctx, str_a);
        JS_FreeValue(ctx, str_b);
        return result;
    }
    // Fallback: numeric addition
    double da, db;
    if (JS_ToFloat64(ctx, &da, a)) return JS_EXCEPTION;
    if (JS_ToFloat64(ctx, &db, b)) return JS_EXCEPTION;
    return JS_NewFloat64(ctx, da + db);
}

JSValue frozen_sub(JSContext *ctx, JSValue a, JSValue b) {
    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
        int64_t r = (int64_t)JS_VALUE_GET_INT(a) - JS_VALUE_GET_INT(b);
        if (likely((int32_t)r == r)) return JS_MKVAL(JS_TAG_INT, (int32_t)r);
        return JS_NewFloat64(ctx, (double)r);
    }
    double da, db;
    if (JS_ToFloat64(ctx, &da, a)) return JS_EXCEPTION;
    if (JS_ToFloat64(ctx, &db, b)) return JS_EXCEPTION;
    return JS_NewFloat64(ctx, da - db);
}

JSValue frozen_mul(JSContext *ctx, JSValue a, JSValue b) {
    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
        int64_t r = (int64_t)JS_VALUE_GET_INT(a) * JS_VALUE_GET_INT(b);
        if (likely((int32_t)r == r)) return JS_MKVAL(JS_TAG_INT, (int32_t)r);
        return JS_NewFloat64(ctx, (double)r);
    }
    double da, db;
    if (JS_ToFloat64(ctx, &da, a)) return JS_EXCEPTION;
    if (JS_ToFloat64(ctx, &db, b)) return JS_EXCEPTION;
    return JS_NewFloat64(ctx, da * db);
}

JSValue frozen_div(JSContext *ctx, JSValue a, JSValue b) {
    double da, db;
    if (JS_ToFloat64(ctx, &da, a)) return JS_EXCEPTION;
    if (JS_ToFloat64(ctx, &db, b)) return JS_EXCEPTION;
    return JS_NewFloat64(ctx, da / db);
}

JSValue frozen_mod(JSContext *ctx, JSValue a, JSValue b) {
    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
        int32_t ia = JS_VALUE_GET_INT(a);
        int32_t ib = JS_VALUE_GET_INT(b);
        if (unlikely(ib == 0)) return JS_NewFloat64(ctx, NAN);
        if (unlikely(ib == -1 && ia == INT32_MIN)) return JS_MKVAL(JS_TAG_INT, 0);
        return JS_MKVAL(JS_TAG_INT, ia % ib);
    }
    double da, db;
    if (JS_ToFloat64(ctx, &da, a)) return JS_EXCEPTION;
    if (JS_ToFloat64(ctx, &db, b)) return JS_EXCEPTION;
    return JS_NewFloat64(ctx, fmod(da, db));
}

JSValue frozen_neg(JSContext *ctx, JSValue a) {
    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT)) {
        int32_t ia = JS_VALUE_GET_INT(a);
        // Special case: -0 must return float -0.0, not int 0
        if (unlikely(ia == 0)) return JS_NewFloat64(ctx, -0.0);
        if (unlikely(ia == INT32_MIN)) return JS_NewFloat64(ctx, -(double)ia);
        return JS_MKVAL(JS_TAG_INT, -ia);
    }
    double da;
    if (JS_ToFloat64(ctx, &da, a)) return JS_EXCEPTION;
    return JS_NewFloat64(ctx, -da);
}

/* ============================================================================
 * Comparison Helpers
 * Return C int (0/1) for use in conditionals
 * ============================================================================ */

/* Slow path comparison functions - called when operands are not both ints */
int frozen_lt_slow(JSContext *ctx, JSValue a, JSValue b) {
    double da, db;
    if (JS_ToFloat64(ctx, &da, a)) return 0;
    if (JS_ToFloat64(ctx, &db, b)) return 0;
    return da < db;
}

int frozen_lte_slow(JSContext *ctx, JSValue a, JSValue b) {
    double da, db;
    if (JS_ToFloat64(ctx, &da, a)) return 0;
    if (JS_ToFloat64(ctx, &db, b)) return 0;
    return da <= db;
}

int frozen_gt_slow(JSContext *ctx, JSValue a, JSValue b) {
    double da, db;
    if (JS_ToFloat64(ctx, &da, a)) return 0;
    if (JS_ToFloat64(ctx, &db, b)) return 0;
    return da > db;
}

int frozen_gte_slow(JSContext *ctx, JSValue a, JSValue b) {
    double da, db;
    if (JS_ToFloat64(ctx, &da, a)) return 0;
    if (JS_ToFloat64(ctx, &db, b)) return 0;
    return da >= db;
}

int frozen_eq_slow(JSContext *ctx, JSValue a, JSValue b) {
    (void)ctx;
    /* For non-SMI, use tag+ptr comparison */
    return JS_VALUE_GET_TAG(a) == JS_VALUE_GET_TAG(b) &&
           JS_VALUE_GET_PTR(a) == JS_VALUE_GET_PTR(b);
}

int frozen_neq_slow(JSContext *ctx, JSValue a, JSValue b) {
    return !frozen_eq_slow(ctx, a, b);
}

/* ============================================================================
 * Bitwise Operation Helpers
 * ============================================================================ */

JSValue frozen_and(JSContext *ctx, JSValue a, JSValue b) {
    int32_t ia, ib;
    if (JS_ToInt32(ctx, &ia, a)) return JS_EXCEPTION;
    if (JS_ToInt32(ctx, &ib, b)) return JS_EXCEPTION;
    return JS_MKVAL(JS_TAG_INT, ia & ib);
}

JSValue frozen_or(JSContext *ctx, JSValue a, JSValue b) {
    int32_t ia, ib;
    if (JS_ToInt32(ctx, &ia, a)) return JS_EXCEPTION;
    if (JS_ToInt32(ctx, &ib, b)) return JS_EXCEPTION;
    return JS_MKVAL(JS_TAG_INT, ia | ib);
}

JSValue frozen_xor(JSContext *ctx, JSValue a, JSValue b) {
    int32_t ia, ib;
    if (JS_ToInt32(ctx, &ia, a)) return JS_EXCEPTION;
    if (JS_ToInt32(ctx, &ib, b)) return JS_EXCEPTION;
    return JS_MKVAL(JS_TAG_INT, ia ^ ib);
}

JSValue frozen_shl(JSContext *ctx, JSValue a, JSValue b) {
    int32_t ia, ib;
    if (JS_ToInt32(ctx, &ia, a)) return JS_EXCEPTION;
    if (JS_ToInt32(ctx, &ib, b)) return JS_EXCEPTION;
    return JS_MKVAL(JS_TAG_INT, ia << (ib & 0x1f));
}

JSValue frozen_sar(JSContext *ctx, JSValue a, JSValue b) {
    int32_t ia, ib;
    if (JS_ToInt32(ctx, &ia, a)) return JS_EXCEPTION;
    if (JS_ToInt32(ctx, &ib, b)) return JS_EXCEPTION;
    return JS_MKVAL(JS_TAG_INT, ia >> (ib & 0x1f));
}

JSValue frozen_shr(JSContext *ctx, JSValue a, JSValue b) {
    uint32_t ua;
    int32_t ib;
    if (JS_ToInt32(ctx, (int32_t*)&ua, a)) return JS_EXCEPTION;
    if (JS_ToInt32(ctx, &ib, b)) return JS_EXCEPTION;
    uint32_t result = ua >> (ib & 0x1f);
    /* Unsigned shift may produce value > INT32_MAX */
    if (result > INT32_MAX) return JS_NewFloat64(ctx, (double)result);
    return JS_MKVAL(JS_TAG_INT, (int32_t)result);
}

JSValue frozen_not(JSContext *ctx, JSValue a) {
    int32_t ia;
    if (JS_ToInt32(ctx, &ia, a)) return JS_EXCEPTION;
    return JS_MKVAL(JS_TAG_INT, ~ia);
}

/* ============================================================================
 * Partial Freeze Fallback Registry
 * Stores JSValue function references for interpreter fallback from frozen code.
 * The bytecode array is currently unused - we save functions from globalThis instead.
 * ============================================================================ */

#define FROZEN_MAX_FALLBACK_ENTRIES 64

typedef struct {
    const char *func_name;
    JSValue cached_func;  /* The original bytecode function saved before replacement */
    int is_valid;
} FrozenFallbackEntry;

static FrozenFallbackEntry frozen_fallback_registry[FROZEN_MAX_FALLBACK_ENTRIES];
static int frozen_fallback_count = 0;

/* Register bytecode - currently unused but kept for future use */
int frozen_register_bytecode(const char *func_name, const uint8_t *bytecode, size_t bytecode_len) {
    (void)bytecode;
    (void)bytecode_len;
    if (frozen_fallback_count >= FROZEN_MAX_FALLBACK_ENTRIES) {
        return -1;  /* Registry full */
    }
    FrozenFallbackEntry *entry = &frozen_fallback_registry[frozen_fallback_count++];
    entry->func_name = func_name;
    entry->cached_func = JS_UNDEFINED;
    entry->is_valid = 0;
    return 0;
}

/* Save the original function before it gets replaced */
void frozen_save_original_func(const char *func_name, JSValue func) {
    for (int i = 0; i < frozen_fallback_count; i++) {
        if (strcmp(frozen_fallback_registry[i].func_name, func_name) == 0) {
            frozen_fallback_registry[i].cached_func = JS_DupValue(NULL, func); /* NULL ctx ok for DupValue */
            frozen_fallback_registry[i].is_valid = 1;
            return;
        }
    }
}

/* Find entry by function name */
static FrozenFallbackEntry *frozen_find_fallback(const char *func_name) {
    for (int i = 0; i < frozen_fallback_count; i++) {
        if (strcmp(frozen_fallback_registry[i].func_name, func_name) == 0) {
            return &frozen_fallback_registry[i];
        }
    }
    return NULL;
}

/**
 * Convert locals array to JS array for heap storage
 * Used when transitioning from frozen to interpreter for contaminated blocks
 */
JSValue frozen_locals_to_array(JSContext *ctx, JSValue *locals, int num_locals) {
    JSValue arr = JS_NewArray(ctx);
    if (JS_IsException(arr)) return arr;

    for (int i = 0; i < num_locals; i++) {
        /* Dup because array takes ownership */
        if (JS_SetPropertyUint32(ctx, arr, i, JS_DupValue(ctx, locals[i])) < 0) {
            JS_FreeValue(ctx, arr);
            return JS_EXCEPTION;
        }
    }
    return arr;
}

/**
 * Restore locals array from JS array after interpreter execution
 * Frees old values and takes ownership of new values
 */
int frozen_array_to_locals(JSContext *ctx, JSValue arr, JSValue *locals, int num_locals) {
    if (!JS_IsArray(arr)) {
        return -1;
    }

    for (int i = 0; i < num_locals; i++) {
        JSValue val = JS_GetPropertyUint32(ctx, arr, i);
        if (JS_IsException(val)) {
            return -1;
        }
        /* Free old value and replace with new */
        JS_FreeValue(ctx, locals[i]);
        locals[i] = val; /* Takes ownership */
    }
    return 0;
}

JSValue frozen_fallback_call(JSContext *ctx, const char *func_name,
                             JSValue this_val, int argc, JSValue *argv) {
    FrozenFallbackEntry *entry = frozen_find_fallback(func_name);
    if (!entry) {
        return JS_ThrowReferenceError(ctx, "frozen_fallback_call: function '%s' not registered", func_name);
    }

    /* Lazy load: if not yet saved, try to get from globalThis now */
    if (!entry->is_valid) {
        JSValue global = JS_GetGlobalObject(ctx);
        /* Try __original_{name} first (saved by hook injection) */
        char original_name[256];
        snprintf(original_name, sizeof(original_name), "__original_%s", func_name);
        JSValue func = JS_GetPropertyStr(ctx, global, original_name);
        if (JS_IsUndefined(func)) {
            /* Try without prefix as fallback */
            func = JS_GetPropertyStr(ctx, global, func_name);
        }
        JS_FreeValue(ctx, global);

        if (JS_IsFunction(ctx, func)) {
            entry->cached_func = func; /* Takes ownership */
            entry->is_valid = 1;
        } else {
            JS_FreeValue(ctx, func);
            return JS_ThrowReferenceError(ctx, "frozen_fallback_call: function '%s' not found in globalThis", func_name);
        }
    }

    /* Set re-entry flag in JS globalThis to prevent infinite loop */
    /* The hook injection checks globalThis.__frozen_fallback_active before redirecting */
    JSValue global = JS_GetGlobalObject(ctx);
    JS_SetPropertyStr(ctx, global, "__frozen_fallback_active", JS_NewBool(ctx, 1));
    frozen_fallback_active = 1;

    /* Call the cached original bytecode function */
    JSValue result = JS_Call(ctx, entry->cached_func, this_val, argc, argv);

    /* Clear re-entry flag */
    JS_SetPropertyStr(ctx, global, "__frozen_fallback_active", JS_NewBool(ctx, 0));
    JS_FreeValue(ctx, global);
    frozen_fallback_active = 0;

    return result;
}

/**
 * Block-level fallback: execute contaminated block in interpreter with locals preservation
 *
 * The interpreter function receives: (original_args..., locals_array, block_id, stack_array)
 * It returns: { locals: [...], stack: [...], next_block: N, return_value: ... }
 *
 * If return_value is present, the function returned early - we return it immediately.
 * Otherwise, we restore locals/stack and jump to next_block.
 */
JSValue frozen_block_fallback(JSContext *ctx, const char *func_name,
                               JSValue this_val, int argc, JSValue *argv,
                               JSValue *locals, int num_locals,
                               JSValue *stack, int *sp,
                               int block_id, int *next_block_out) {
    /* Create locals array */
    JSValue locals_arr = frozen_locals_to_array(ctx, locals, num_locals);
    if (JS_IsException(locals_arr)) return locals_arr;

    /* Create stack array (only include current stack) */
    JSValue stack_arr = JS_NewArray(ctx);
    if (JS_IsException(stack_arr)) {
        JS_FreeValue(ctx, locals_arr);
        return stack_arr;
    }
    for (int i = 0; i < *sp; i++) {
        if (JS_SetPropertyUint32(ctx, stack_arr, i, JS_DupValue(ctx, stack[i])) < 0) {
            JS_FreeValue(ctx, locals_arr);
            JS_FreeValue(ctx, stack_arr);
            return JS_EXCEPTION;
        }
    }

    /* Build extended argv: [original_args..., locals, block_id, stack] */
    int extended_argc = argc + 3;
    JSValue *extended_argv = js_malloc(ctx, sizeof(JSValue) * extended_argc);
    if (!extended_argv) {
        JS_FreeValue(ctx, locals_arr);
        JS_FreeValue(ctx, stack_arr);
        return JS_EXCEPTION;
    }

    for (int i = 0; i < argc; i++) {
        extended_argv[i] = JS_DupValue(ctx, argv[i]);
    }
    extended_argv[argc] = locals_arr;  /* Takes ownership */
    extended_argv[argc + 1] = JS_NewInt32(ctx, block_id);
    extended_argv[argc + 2] = stack_arr; /* Takes ownership */

    /* Call __block_fallback_{func_name} */
    char fallback_name[256];
    snprintf(fallback_name, sizeof(fallback_name), "__block_fallback_%s", func_name);

    JSValue global = JS_GetGlobalObject(ctx);
    JSValue fallback_func = JS_GetPropertyStr(ctx, global, fallback_name);
    JS_FreeValue(ctx, global);

    if (!JS_IsFunction(ctx, fallback_func)) {
        /* No block fallback available - fall back to full function */
        for (int i = 0; i < extended_argc; i++) {
            JS_FreeValue(ctx, extended_argv[i]);
        }
        js_free(ctx, extended_argv);
        JS_FreeValue(ctx, fallback_func);
        return frozen_fallback_call(ctx, func_name, this_val, argc, argv);
    }

    /* Set fallback active flag */
    frozen_fallback_active = 1;

    /* Call block fallback */
    JSValue result_obj = JS_Call(ctx, fallback_func, this_val, extended_argc, extended_argv);

    /* Cleanup extended argv */
    for (int i = 0; i < extended_argc; i++) {
        JS_FreeValue(ctx, extended_argv[i]);
    }
    js_free(ctx, extended_argv);
    JS_FreeValue(ctx, fallback_func);
    frozen_fallback_active = 0;

    if (JS_IsException(result_obj)) {
        return result_obj;
    }

    /* Check if function returned early (has return_value property) */
    JSValue return_val = JS_GetPropertyStr(ctx, result_obj, "return_value");
    if (!JS_IsUndefined(return_val)) {
        JS_FreeValue(ctx, result_obj);
        return return_val; /* Early return from function */
    }
    JS_FreeValue(ctx, return_val);

    /* Restore locals from result */
    JSValue new_locals = JS_GetPropertyStr(ctx, result_obj, "locals");
    if (!JS_IsException(new_locals) && JS_IsArray(new_locals)) {
        frozen_array_to_locals(ctx, new_locals, locals, num_locals);
    }
    JS_FreeValue(ctx, new_locals);

    /* Restore stack from result */
    JSValue new_stack = JS_GetPropertyStr(ctx, result_obj, "stack");
    if (!JS_IsException(new_stack) && JS_IsArray(new_stack)) {
        /* Clear old stack */
        for (int i = 0; i < *sp; i++) {
            JS_FreeValue(ctx, stack[i]);
        }
        /* Get new stack length */
        JSValue len_val = JS_GetPropertyStr(ctx, new_stack, "length");
        int32_t new_sp = 0;
        if (JS_ToInt32(ctx, &new_sp, len_val) >= 0) {
            *sp = new_sp;
            for (int i = 0; i < new_sp; i++) {
                stack[i] = JS_GetPropertyUint32(ctx, new_stack, i);
            }
        }
        JS_FreeValue(ctx, len_val);
    }
    JS_FreeValue(ctx, new_stack);

    /* Get next block to jump to */
    JSValue next_block_val = JS_GetPropertyStr(ctx, result_obj, "next_block");
    if (!JS_IsUndefined(next_block_val)) {
        int32_t next_block = 0;
        if (JS_ToInt32(ctx, &next_block, next_block_val) >= 0) {
            *next_block_out = next_block;
        }
    }
    JS_FreeValue(ctx, next_block_val);
    JS_FreeValue(ctx, result_obj);

    /* Return undefined to indicate continue execution */
    return JS_UNDEFINED;
}

/* ============================================================================
 * Module System Support for Frozen Functions
 * ============================================================================ */

/**
 * Dynamic import with explicit basename for frozen functions.
 * Since frozen C functions don't have bytecode stack frames, we need to
 * explicitly pass the module basename for relative path resolution.
 *
 * @param ctx QuickJS context
 * @param specifier Module specifier (the import path)
 * @param basename The calling module's filename (for relative imports)
 * @return Promise that resolves to the module namespace
 */
/* Forward declare WASM component loader from Zig */
extern JSValue __edgebox_load_wasm_component(JSContext *ctx, const char *path, size_t path_len);

JSValue frozen_dynamic_import(JSContext *ctx, JSValueConst specifier, const char *basename) {
    extern JSValue js_dynamic_import(JSContext *ctx, JSValueConst specifier);

    /* Check if this is a WASM import */
    const char *spec_str = JS_ToCString(ctx, specifier);
    if (spec_str) {
        size_t len = strlen(spec_str);
        if (len > 5 && strcmp(spec_str + len - 5, ".wasm") == 0) {
            /* This is a WASM component - load it and wrap in resolved Promise */
            JSValue namespace = __edgebox_load_wasm_component(ctx, spec_str, len);
            JS_FreeCString(ctx, spec_str);

            /* If loading failed (exception), return the exception */
            if (JS_IsException(namespace)) {
                return namespace;
            }

            /* Wrap the namespace in a resolved Promise to match import() semantics */
            JSValue promise = JS_NewPromiseCapability(ctx, NULL);
            if (JS_IsException(promise)) {
                JS_FreeValue(ctx, namespace);
                return promise;
            }

            /* Resolve the promise immediately with the namespace */
            JSValue resolve_func = JS_GetPropertyStr(ctx, promise, "resolve");
            if (JS_IsFunction(ctx, resolve_func)) {
                JSValue args[1] = { namespace };
                JSValue ret = JS_Call(ctx, resolve_func, JS_UNDEFINED, 1, args);
                JS_FreeValue(ctx, ret);
            }
            JS_FreeValue(ctx, resolve_func);
            JS_FreeValue(ctx, namespace);

            /* Return the promise property instead of the capability object */
            JSValue actual_promise = JS_GetPropertyStr(ctx, promise, "promise");
            JS_FreeValue(ctx, promise);
            return actual_promise;
        }
        JS_FreeCString(ctx, spec_str);
    }

    /* Regular JS module import */
    (void)basename; /* basename not used, js_dynamic_import gets it via JS_GetScriptOrModuleName */
    return js_dynamic_import(ctx, specifier);
}

/* ============================================================================
 * Native Shape Registry - Zero-Overhead Property Access for AST Nodes
 *
 * This registry maps JSValue addresses to native AstNode pointers.
 * When frozen code accesses node.kind, we check the registry first.
 * If found, we read directly from native memory (1 cycle).
 * If not found, we fall back to QuickJS (35 cycles).
 * ============================================================================ */

#include "native_shapes.h"

#define NATIVE_REGISTRY_SIZE 65536  /* 64K entries, ~512KB */

typedef struct {
    uint64_t js_addr;
    NativeAstNode *node;
} RegistryEntry;

/* Global registry - simple hash table */
static RegistryEntry *native_registry = NULL;
static NativeAstNode *node_pool = NULL;
static int node_pool_size = 0;
static int node_pool_cap = 0;

/* Initialize the native registry */
void native_registry_init(void) {
    if (!native_registry) {
        native_registry = (RegistryEntry*)calloc(NATIVE_REGISTRY_SIZE, sizeof(RegistryEntry));
        node_pool_cap = 16384;  /* Start with 16K nodes */
        node_pool = (NativeAstNode*)malloc(node_pool_cap * sizeof(NativeAstNode));
        node_pool_size = 0;
    }
}

/* Clean up the native registry */
void native_registry_deinit(void) {
    if (native_registry) {
        free(native_registry);
        native_registry = NULL;
    }
    if (node_pool) {
        free(node_pool);
        node_pool = NULL;
        node_pool_size = 0;
        node_pool_cap = 0;
    }
}

/* Simple hash function for JSValue addresses */
static inline uint32_t hash_addr(uint64_t addr) {
    return (uint32_t)((addr >> 3) ^ (addr >> 17)) & (NATIVE_REGISTRY_SIZE - 1);
}

/* Debug: track registration */
uint64_t debug_last_registered_addr = 0;
int debug_register_success = 0;

/* Debug: track what register32 receives */
uint32_t debug_register32_addr = 0;
int debug_register32_called = 0;

/* Register a native node using 32-bit address (WASM ABI compatible) */
__attribute__((noinline))
NativeAstNode* native_node_register32(uint32_t js_addr32, int32_t kind, int32_t flags, int32_t pos, int32_t end) {
    /* Track that this 32-bit version was called */
    debug_register32_called++;
    debug_register32_addr = js_addr32;

    /* Debug: print to stderr to confirm this function is called */
    fprintf(stderr, "[DEBUG] native_node_register32 called with addr32=%u kind=%d\n", js_addr32, kind);

    uint64_t js_addr = (uint64_t)js_addr32;
    debug_last_registered_addr = js_addr;
    debug_register_success = 0;

    if (!native_registry) return NULL;

    /* Grow pool if needed */
    if (node_pool_size >= node_pool_cap) {
        int new_cap = node_pool_cap * 2;
        NativeAstNode *new_pool = (NativeAstNode*)realloc(node_pool, new_cap * sizeof(NativeAstNode));
        if (!new_pool) return NULL;
        node_pool = new_pool;
        node_pool_cap = new_cap;
    }

    /* Allocate node from pool */
    NativeAstNode *node = &node_pool[node_pool_size++];
    node->kind = kind;
    node->flags = flags;
    node->pos = pos;
    node->end = end;
    node->parent = NULL;
    node->js_value = js_addr;
    node->modifier_flags_cache = 0;
    node->transform_flags = 0;

    /* Insert into registry with linear probing */
    uint32_t idx = hash_addr(js_addr);
    for (int i = 0; i < 16; i++) {  /* Max 16 probes */
        uint32_t slot = (idx + i) & (NATIVE_REGISTRY_SIZE - 1);
        if (native_registry[slot].js_addr == 0) {
            native_registry[slot].js_addr = js_addr;
            native_registry[slot].node = node;
            debug_register_success = 1;
            return node;
        }
    }

    /* Registry full - degrade gracefully */
    node_pool_size--;  /* Return node to pool */
    return NULL;
}

/* Register a native node for a JSValue */
NativeAstNode* native_node_register(uint64_t js_addr, int32_t kind, int32_t flags, int32_t pos, int32_t end) {
    debug_last_registered_addr = js_addr;
    debug_register_success = 0;

    if (!native_registry) return NULL;

    /* Grow pool if needed */
    if (node_pool_size >= node_pool_cap) {
        int new_cap = node_pool_cap * 2;
        NativeAstNode *new_pool = (NativeAstNode*)realloc(node_pool, new_cap * sizeof(NativeAstNode));
        if (!new_pool) return NULL;
        node_pool = new_pool;
        node_pool_cap = new_cap;
    }

    /* Allocate node from pool */
    NativeAstNode *node = &node_pool[node_pool_size++];
    node->kind = kind;
    node->flags = flags;
    node->pos = pos;
    node->end = end;
    node->parent = NULL;
    node->js_value = js_addr;
    node->modifier_flags_cache = 0;
    node->transform_flags = 0;

    /* Insert into registry with linear probing */
    uint32_t idx = hash_addr(js_addr);
    for (int i = 0; i < 16; i++) {  /* Max 16 probes */
        uint32_t slot = (idx + i) & (NATIVE_REGISTRY_SIZE - 1);
        if (native_registry[slot].js_addr == 0) {
            native_registry[slot].js_addr = js_addr;
            native_registry[slot].node = node;
            debug_register_success = 1;
            return node;
        }
    }

    /* Registry full - degrade gracefully */
    node_pool_size--;  /* Return node to pool */
    return NULL;
}

/* Debug accessors */
uint64_t native_debug_get_registered_addr(void) { return debug_last_registered_addr; }
int native_debug_get_register_success(void) { return debug_register_success; }

/* Debug accessors for 32-bit version */
uint32_t native_debug_get_register32_addr(void) { return debug_register32_addr; }
int native_debug_get_register32_called(void) { return debug_register32_called; }

/* Fast lookup for native node */
NativeAstNode* native_node_lookup(uint64_t js_addr) {
    if (!native_registry || js_addr == 0) return NULL;

    uint32_t idx = hash_addr(js_addr);
    for (int i = 0; i < 16; i++) {  /* Max 16 probes */
        uint32_t slot = (idx + i) & (NATIVE_REGISTRY_SIZE - 1);
        if (native_registry[slot].js_addr == js_addr) {
            return native_registry[slot].node;
        }
        if (native_registry[slot].js_addr == 0) {
            return NULL;  /* Empty slot = not found */
        }
    }
    return NULL;
}

/* Fast property accessors */
int32_t native_get_kind(NativeAstNode *node) { return node->kind; }
int32_t native_get_flags(NativeAstNode *node) { return node->flags; }
int32_t native_get_pos(NativeAstNode *node) { return node->pos; }
int32_t native_get_end(NativeAstNode *node) { return node->end; }
NativeAstNode* native_get_parent(NativeAstNode *node) { return node->parent; }

/* Set parent relationship */
void native_node_set_parent(NativeAstNode *child, NativeAstNode *parent) {
    if (child) child->parent = parent;
}

/* Get stats for debugging */
int native_registry_count(void) { return node_pool_size; }

/* Debug: Get last lookup address */
uint64_t debug_last_lookup_addr = 0;
int debug_lookup_found = 0;
int debug_lookup_call_count = 0;

void native_debug_log_lookup(uint64_t addr, int found) {
    debug_last_lookup_addr = addr;
    debug_lookup_found = found;
    debug_lookup_call_count++;
}

uint64_t native_debug_get_last_lookup(void) {
    return debug_last_lookup_addr;
}

int native_debug_get_found(void) {
    return debug_lookup_found;
}

int native_debug_get_call_count(void) {
    return debug_lookup_call_count;
}
