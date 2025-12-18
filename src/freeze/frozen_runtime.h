/*
 * Frozen Runtime Header - Extern declarations for shared helpers
 * Include this in frozen_user.c to call pre-compiled helpers from frozen_runtime.o
 */

#ifndef FROZEN_RUNTIME_H
#define FROZEN_RUNTIME_H

#include "quickjs.h"
#include <stdint.h>
#include <math.h>

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

/* Global call depth counter - extern, defined in frozen_runtime.c */
extern int frozen_call_depth;

/* Re-entry flag for partial freeze fallback - when set, hooks should NOT redirect */
extern int frozen_fallback_active;

/* Reset call depth (call at start of each request) */
void frozen_reset_call_depth(void);

/* Stack overflow check macro - returns RangeError like Node.js */
#define FROZEN_CHECK_STACK(ctx) do { \
    if (unlikely(frozen_call_depth >= FROZEN_MAX_CALL_DEPTH)) { \
        return JS_ThrowRangeError(ctx, "Maximum call stack size exceeded"); \
    } \
    frozen_call_depth++; \
} while(0)
#define FROZEN_EXIT_STACK() (frozen_call_depth--)

/* Stack operations with bounds checking */
#define PUSH(v) do { \
    if (unlikely(sp >= max_stack)) { \
        FROZEN_EXIT_STACK(); \
        return JS_ThrowRangeError(ctx, "Operand stack overflow"); \
    } \
    stack[sp++] = (v); \
} while(0)
#define POP() (stack[--sp])
#define TOP() (stack[sp-1])
#define SET_TOP(v) (stack[sp-1] = (v))

/* SMI-optimized dup/free - skip refcount for immediate values.
   Both macros evaluate their argument only once using compound statements.
   This is critical when arguments have side effects (like POP()). */
#define FROZEN_DUP(ctx, v) ({ JSValue _fd_tmp = (v); JS_VALUE_HAS_REF_COUNT(_fd_tmp) ? JS_DupValue(ctx, _fd_tmp) : _fd_tmp; })
#define FROZEN_FREE(ctx, v) do { JSValue _ff_tmp = (v); if (JS_VALUE_HAS_REF_COUNT(_ff_tmp)) JS_FreeValue(ctx, _ff_tmp); } while(0)

/* SMI arithmetic helpers - extern, defined in frozen_runtime.c */
JSValue frozen_add(JSContext *ctx, JSValue a, JSValue b);
JSValue frozen_sub(JSContext *ctx, JSValue a, JSValue b);
JSValue frozen_mul(JSContext *ctx, JSValue a, JSValue b);
JSValue frozen_div(JSContext *ctx, JSValue a, JSValue b);
JSValue frozen_mod(JSContext *ctx, JSValue a, JSValue b);
JSValue frozen_neg(JSContext *ctx, JSValue a);

/* Comparison helpers - extern, defined in frozen_runtime.c */
int frozen_lt(JSContext *ctx, JSValue a, JSValue b);
int frozen_lte(JSContext *ctx, JSValue a, JSValue b);
int frozen_gt(JSContext *ctx, JSValue a, JSValue b);
int frozen_gte(JSContext *ctx, JSValue a, JSValue b);
int frozen_eq(JSContext *ctx, JSValue a, JSValue b);
int frozen_neq(JSContext *ctx, JSValue a, JSValue b);

/* Bitwise helpers - extern, defined in frozen_runtime.c */
JSValue frozen_and(JSContext *ctx, JSValue a, JSValue b);
JSValue frozen_or(JSContext *ctx, JSValue a, JSValue b);
JSValue frozen_xor(JSContext *ctx, JSValue a, JSValue b);
JSValue frozen_shl(JSContext *ctx, JSValue a, JSValue b);
JSValue frozen_sar(JSContext *ctx, JSValue a, JSValue b);
JSValue frozen_shr(JSContext *ctx, JSValue a, JSValue b);
JSValue frozen_not(JSContext *ctx, JSValue a);

/* Array access helpers - inline for speed */
static inline JSValue frozen_array_get(JSContext *ctx, JSValue obj, JSValue idx) {
    if (JS_VALUE_GET_TAG(idx) == JS_TAG_INT) {
        int32_t i = JS_VALUE_GET_INT(idx);
        if (i >= 0) return JS_GetPropertyUint32(ctx, obj, (uint32_t)i);
        return JS_GetPropertyInt64(ctx, obj, i);
    }
    JSAtom atom = JS_ValueToAtom(ctx, idx);
    if (atom == JS_ATOM_NULL) return JS_EXCEPTION;
    JSValue val = JS_GetProperty(ctx, obj, atom);
    JS_FreeAtom(ctx, atom);
    return val;
}

static inline int frozen_array_set(JSContext *ctx, JSValue obj, JSValue idx, JSValue val) {
    if (JS_VALUE_GET_TAG(idx) == JS_TAG_INT) {
        return JS_SetPropertyInt64(ctx, obj, JS_VALUE_GET_INT(idx), val);
    }
    JSAtom atom = JS_ValueToAtom(ctx, idx);
    if (atom == JS_ATOM_NULL) return -1;
    int r = JS_SetProperty(ctx, obj, atom, val);
    JS_FreeAtom(ctx, atom);
    return r;
}

static inline int64_t frozen_get_length(JSContext *ctx, JSValue obj) {
    int64_t len = 0;
    JS_GetLength(ctx, obj, &len);
    return len;
}

/* Power and typeof helpers - inline for speed */
static inline JSValue frozen_pow(JSContext *ctx, JSValue a, JSValue b) {
    double da, db;
    if (JS_ToFloat64(ctx, &da, a)) return JS_EXCEPTION;
    if (JS_ToFloat64(ctx, &db, b)) return JS_EXCEPTION;
    return JS_NewFloat64(ctx, pow(da, db));
}

static inline JSValue frozen_typeof(JSContext *ctx, JSValue v) {
    const char *s;
    int tag = JS_VALUE_GET_TAG(v);
    switch(tag) {
    case JS_TAG_UNDEFINED: s = "undefined"; break;
    case JS_TAG_NULL: s = "object"; break;
    case JS_TAG_STRING: s = "string"; break;
    case JS_TAG_INT: case JS_TAG_FLOAT64: s = "number"; break;
    case JS_TAG_BOOL: s = "boolean"; break;
    case JS_TAG_BIG_INT: s = "bigint"; break;
    case JS_TAG_SYMBOL: s = "symbol"; break;
    case JS_TAG_OBJECT:
        if (JS_IsFunction(ctx, v)) s = "function";
        else s = "object";
        break;
    default: s = "unknown"; break;
    }
    return JS_NewString(ctx, s);
}

static inline int frozen_in(JSContext *ctx, JSValue key, JSValue obj) {
    JSAtom atom = JS_ValueToAtom(ctx, key);
    if (atom == JS_ATOM_NULL) return -1;
    int r = JS_HasProperty(ctx, obj, atom);
    JS_FreeAtom(ctx, atom);
    return r;
}

/* SIMD helpers - only available in WASM builds */
#ifdef __wasm__
#include <wasm_simd128.h>

static inline int64_t frozen_sum_int32_array_simd(JSContext *ctx, JSValue arr, int64_t len) {
    v128_t sum_vec = wasm_i32x4_splat(0);
    int64_t i = 0;

    for (; i + 4 <= len; i += 4) {
        int32_t vals[4];
        int all_int32 = 1;

        for (int j = 0; j < 4; j++) {
            JSValue val = JS_GetPropertyUint32(ctx, arr, (uint32_t)(i + j));
            if (likely(JS_VALUE_GET_TAG(val) == JS_TAG_INT)) {
                vals[j] = JS_VALUE_GET_INT(val);
            } else {
                JS_FreeValue(ctx, val);
                all_int32 = 0;
                break;
            }
        }

        if (all_int32) {
            v128_t vec = wasm_v128_load(vals);
            sum_vec = wasm_i32x4_add(sum_vec, vec);
        } else {
            return -1;
        }
    }

    int32_t partial[4];
    wasm_v128_store(partial, sum_vec);
    int64_t sum = (int64_t)partial[0] + partial[1] + partial[2] + partial[3];

    for (; i < len; i++) {
        JSValue val = JS_GetPropertyUint32(ctx, arr, (uint32_t)i);
        if (likely(JS_VALUE_GET_TAG(val) == JS_TAG_INT)) {
            sum += JS_VALUE_GET_INT(val);
        } else {
            JS_FreeValue(ctx, val);
            return -1;
        }
    }

    return sum;
}
#endif /* __wasm__ */

/* ============================================================================
 * Partial Freeze Bytecode Fallback
 * When frozen code hits a contaminated block, it calls back to the interpreter
 * via embedded bytecode rather than relying on globalThis lookup.
 * ============================================================================ */

/**
 * Register a function's bytecode for fallback execution.
 * Called during frozen_init for partial freeze functions.
 *
 * @param func_name The function name (used as key for lookup)
 * @param bytecode Pointer to the function's bytecode
 * @param bytecode_len Length of the bytecode
 * @return 0 on success, -1 on error
 */
int frozen_register_bytecode(const char *func_name, const uint8_t *bytecode, size_t bytecode_len);

/**
 * Save the original bytecode function before replacing it with frozen version.
 * This allows fallback to the original function when contaminated blocks are hit.
 *
 * @param func_name The function name (must match a registered bytecode entry)
 * @param func The original JSValue function to save
 */
void frozen_save_original_func(const char *func_name, JSValue func);

/**
 * Execute a function via its registered bytecode (fallback from frozen code).
 * Uses cached JSValue function if already loaded, otherwise loads from bytecode.
 *
 * @param ctx QuickJS context
 * @param func_name The function name to look up
 * @param this_val The 'this' value for the call
 * @param argc Number of arguments
 * @param argv Array of arguments
 * @return The function's return value, or JS_EXCEPTION on error
 */
JSValue frozen_fallback_call(JSContext *ctx, const char *func_name,
                             JSValue this_val, int argc, JSValue *argv);

/* ============================================================================
 * Runtime module system support
 * ============================================================================ */

/**
 * Dynamic import - implements ES6 import() expression
 * Returns a Promise that resolves to the module namespace
 */
extern JSValue js_dynamic_import(JSContext *ctx, JSValueConst specifier);

/**
 * Dynamic import with explicit basename for frozen functions.
 * Since frozen C functions don't have bytecode stack frames, we provide
 * the module basename explicitly for relative path resolution.
 *
 * @param ctx QuickJS context
 * @param specifier Module specifier (the import path)
 * @param basename The calling module's filename (for relative imports), or NULL
 * @return Promise that resolves to the module namespace
 */
JSValue frozen_dynamic_import(JSContext *ctx, JSValueConst specifier, const char *basename);

#endif /* FROZEN_RUNTIME_H */
