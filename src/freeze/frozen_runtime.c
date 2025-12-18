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

int frozen_lt(JSContext *ctx, JSValue a, JSValue b) {
    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
        return JS_VALUE_GET_INT(a) < JS_VALUE_GET_INT(b);
    }
    double da, db;
    if (JS_ToFloat64(ctx, &da, a)) return 0;
    if (JS_ToFloat64(ctx, &db, b)) return 0;
    return da < db;
}

int frozen_lte(JSContext *ctx, JSValue a, JSValue b) {
    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
        return JS_VALUE_GET_INT(a) <= JS_VALUE_GET_INT(b);
    }
    double da, db;
    if (JS_ToFloat64(ctx, &da, a)) return 0;
    if (JS_ToFloat64(ctx, &db, b)) return 0;
    return da <= db;
}

int frozen_gt(JSContext *ctx, JSValue a, JSValue b) {
    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
        return JS_VALUE_GET_INT(a) > JS_VALUE_GET_INT(b);
    }
    double da, db;
    if (JS_ToFloat64(ctx, &da, a)) return 0;
    if (JS_ToFloat64(ctx, &db, b)) return 0;
    return da > db;
}

int frozen_gte(JSContext *ctx, JSValue a, JSValue b) {
    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
        return JS_VALUE_GET_INT(a) >= JS_VALUE_GET_INT(b);
    }
    double da, db;
    if (JS_ToFloat64(ctx, &da, a)) return 0;
    if (JS_ToFloat64(ctx, &db, b)) return 0;
    return da >= db;
}

int frozen_eq(JSContext *ctx, JSValue a, JSValue b) {
    if (JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT) {
        return JS_VALUE_GET_INT(a) == JS_VALUE_GET_INT(b);
    }
    /* For non-SMI, use QuickJS comparison */
    return JS_VALUE_GET_TAG(a) == JS_VALUE_GET_TAG(b) &&
           JS_VALUE_GET_PTR(a) == JS_VALUE_GET_PTR(b);
}

int frozen_neq(JSContext *ctx, JSValue a, JSValue b) {
    return !frozen_eq(ctx, a, b);
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
JSValue frozen_dynamic_import(JSContext *ctx, JSValueConst specifier, const char *basename) {
    /* Declare js_dynamic_import_job (static in quickjs.c, need extern) */
    extern JSValue js_dynamic_import_job(JSContext *ctx, int argc, JSValueConst *argv);

    JSValue promise, resolving_funcs[2], basename_val;
    JSValue args[4];

    /* Convert basename string to JSValue */
    if (basename && basename[0] != '\0') {
        basename_val = JS_NewString(ctx, basename);
        if (JS_IsException(basename_val))
            return basename_val;
    } else {
        /* No basename available - this will fail in js_dynamic_import_job */
        /* but that's expected for frozen functions called before init */
        return JS_ThrowTypeError(ctx, "frozen_dynamic_import: basename not set (function called before init?)");
    }

    /* Create Promise for async resolution */
    promise = JS_NewPromiseCapability(ctx, resolving_funcs);
    if (JS_IsException(promise)) {
        JS_FreeValue(ctx, basename_val);
        return promise;
    }

    /* Prepare arguments for dynamic import job */
    args[0] = resolving_funcs[0];  /* resolve function */
    args[1] = resolving_funcs[1];  /* reject function */
    args[2] = basename_val;         /* calling module basename */
    args[3] = specifier;            /* module specifier (already JSValueConst) */

    /* Enqueue async job (cannot load synchronously due to recursion) */
    /* Cast args to JSValueConst* for JS_EnqueueJob */
    JS_EnqueueJob(ctx, js_dynamic_import_job, 4, (JSValueConst *)args);

    JS_FreeValue(ctx, basename_val);
    JS_FreeValue(ctx, resolving_funcs[0]);
    JS_FreeValue(ctx, resolving_funcs[1]);
    return promise;
}
