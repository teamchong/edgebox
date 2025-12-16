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

/* Reset call depth (call at start of each request) */
void frozen_reset_call_depth(void) {
    frozen_call_depth = 0;
}

/* ============================================================================
 * SMI (Small Integer) Arithmetic Helpers
 * Zero allocation fast path for int32 operands
 * Falls back to float64 only on overflow or non-int input
 * ============================================================================ */

JSValue frozen_add(JSContext *ctx, JSValue a, JSValue b) {
    if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
        int64_t r = (int64_t)JS_VALUE_GET_INT(a) + JS_VALUE_GET_INT(b);
        if (likely((int32_t)r == r)) return JS_MKVAL(JS_TAG_INT, (int32_t)r);
        return JS_NewFloat64(ctx, (double)r);
    }
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
