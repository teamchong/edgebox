/*
 * Runtime helpers for frozen functions
 * These are thin wrappers around QuickJS internal functions
 */

#ifndef FREEZE_RUNTIME_HELPERS_H
#define FREEZE_RUNTIME_HELPERS_H

#include "quickjs.h"
#include "cutils.h"

/* Type checking helpers */
static inline int js_is_int(JSValue v) {
    return JS_VALUE_GET_TAG(v) == JS_TAG_INT;
}

static inline int32_t js_get_int(JSValue v) {
    return JS_VALUE_GET_INT(v);
}

static inline int js_is_both_int(JSValue a, JSValue b) {
    return js_is_int(a) && js_is_int(b);
}

/* Slow path arithmetic operations */
/* These need to be implemented or linked from QuickJS */

static inline int js_add_slow(JSContext *ctx, JSValue *sp) {
    JSValue op1 = sp[0];
    JSValue op2 = sp[1];
    JSValue result = JS_UNDEFINED;

    /* Convert to numbers and add */
    double d1, d2;
    if (JS_ToFloat64(ctx, &d1, op1)) {
        JS_FreeValue(ctx, op1);
        JS_FreeValue(ctx, op2);
        return -1;
    }
    if (JS_ToFloat64(ctx, &d2, op2)) {
        JS_FreeValue(ctx, op1);
        JS_FreeValue(ctx, op2);
        return -1;
    }

    result = JS_NewFloat64(ctx, d1 + d2);
    JS_FreeValue(ctx, op1);
    JS_FreeValue(ctx, op2);
    sp[0] = result;
    return 0;
}

static inline int js_sub_slow(JSContext *ctx, JSValue *sp) {
    JSValue op1 = sp[0];
    JSValue op2 = sp[1];

    double d1, d2;
    if (JS_ToFloat64(ctx, &d1, op1) || JS_ToFloat64(ctx, &d2, op2)) {
        JS_FreeValue(ctx, op1);
        JS_FreeValue(ctx, op2);
        return -1;
    }

    JSValue result = JS_NewFloat64(ctx, d1 - d2);
    JS_FreeValue(ctx, op1);
    JS_FreeValue(ctx, op2);
    sp[0] = result;
    return 0;
}

static inline int js_mul_slow(JSContext *ctx, JSValue *sp) {
    JSValue op1 = sp[0];
    JSValue op2 = sp[1];

    double d1, d2;
    if (JS_ToFloat64(ctx, &d1, op1) || JS_ToFloat64(ctx, &d2, op2)) {
        JS_FreeValue(ctx, op1);
        JS_FreeValue(ctx, op2);
        return -1;
    }

    JSValue result = JS_NewFloat64(ctx, d1 * d2);
    JS_FreeValue(ctx, op1);
    JS_FreeValue(ctx, op2);
    sp[0] = result;
    return 0;
}

static inline int js_div_slow(JSContext *ctx, JSValue *sp) {
    JSValue op1 = sp[0];
    JSValue op2 = sp[1];

    double d1, d2;
    if (JS_ToFloat64(ctx, &d1, op1) || JS_ToFloat64(ctx, &d2, op2)) {
        JS_FreeValue(ctx, op1);
        JS_FreeValue(ctx, op2);
        return -1;
    }

    JSValue result = JS_NewFloat64(ctx, d1 / d2);
    JS_FreeValue(ctx, op1);
    JS_FreeValue(ctx, op2);
    sp[0] = result;
    return 0;
}

static inline int js_mod_slow(JSContext *ctx, JSValue *sp) {
    JSValue op1 = sp[0];
    JSValue op2 = sp[1];

    double d1, d2;
    if (JS_ToFloat64(ctx, &d1, op1) || JS_ToFloat64(ctx, &d2, op2)) {
        JS_FreeValue(ctx, op1);
        JS_FreeValue(ctx, op2);
        return -1;
    }

    JSValue result = JS_NewFloat64(ctx, fmod(d1, d2));
    JS_FreeValue(ctx, op1);
    JS_FreeValue(ctx, op2);
    sp[0] = result;
    return 0;
}

/* Comparison slow paths */
static inline int js_lt_slow(JSContext *ctx, JSValue *sp) {
    JSValue op1 = sp[0];
    JSValue op2 = sp[1];

    double d1, d2;
    if (JS_ToFloat64(ctx, &d1, op1) || JS_ToFloat64(ctx, &d2, op2)) {
        JS_FreeValue(ctx, op1);
        JS_FreeValue(ctx, op2);
        return -1;
    }

    JS_FreeValue(ctx, op1);
    JS_FreeValue(ctx, op2);
    sp[0] = JS_NewBool(ctx, d1 < d2);
    return 0;
}

static inline int js_lte_slow(JSContext *ctx, JSValue *sp) {
    JSValue op1 = sp[0];
    JSValue op2 = sp[1];

    double d1, d2;
    if (JS_ToFloat64(ctx, &d1, op1) || JS_ToFloat64(ctx, &d2, op2)) {
        JS_FreeValue(ctx, op1);
        JS_FreeValue(ctx, op2);
        return -1;
    }

    JS_FreeValue(ctx, op1);
    JS_FreeValue(ctx, op2);
    sp[0] = JS_NewBool(ctx, d1 <= d2);
    return 0;
}

static inline int js_gt_slow(JSContext *ctx, JSValue *sp) {
    JSValue op1 = sp[0];
    JSValue op2 = sp[1];

    double d1, d2;
    if (JS_ToFloat64(ctx, &d1, op1) || JS_ToFloat64(ctx, &d2, op2)) {
        JS_FreeValue(ctx, op1);
        JS_FreeValue(ctx, op2);
        return -1;
    }

    JS_FreeValue(ctx, op1);
    JS_FreeValue(ctx, op2);
    sp[0] = JS_NewBool(ctx, d1 > d2);
    return 0;
}

static inline int js_gte_slow(JSContext *ctx, JSValue *sp) {
    JSValue op1 = sp[0];
    JSValue op2 = sp[1];

    double d1, d2;
    if (JS_ToFloat64(ctx, &d1, op1) || JS_ToFloat64(ctx, &d2, op2)) {
        JS_FreeValue(ctx, op1);
        JS_FreeValue(ctx, op2);
        return -1;
    }

    JS_FreeValue(ctx, op1);
    JS_FreeValue(ctx, op2);
    sp[0] = JS_NewBool(ctx, d1 >= d2);
    return 0;
}

static inline int js_eq_slow(JSContext *ctx, JSValue *sp) {
    JSValue op1 = sp[0];
    JSValue op2 = sp[1];

    int result = JS_IsEqual(ctx, op1, op2);
    JS_FreeValue(ctx, op1);
    JS_FreeValue(ctx, op2);

    if (result < 0) return -1;
    sp[0] = JS_NewBool(ctx, result);
    return 0;
}

static inline int js_neq_slow(JSContext *ctx, JSValue *sp) {
    JSValue op1 = sp[0];
    JSValue op2 = sp[1];

    int result = JS_IsEqual(ctx, op1, op2);
    JS_FreeValue(ctx, op1);
    JS_FreeValue(ctx, op2);

    if (result < 0) return -1;
    sp[0] = JS_NewBool(ctx, !result);
    return 0;
}

/* Strict equality check */
static inline int js_strict_eq(JSContext *ctx, JSValue a, JSValue b) {
    (void)ctx;

    int tag_a = JS_VALUE_GET_TAG(a);
    int tag_b = JS_VALUE_GET_TAG(b);

    if (tag_a != tag_b) return 0;

    switch (tag_a) {
    case JS_TAG_INT:
        return JS_VALUE_GET_INT(a) == JS_VALUE_GET_INT(b);
    case JS_TAG_BOOL:
        return JS_VALUE_GET_BOOL(a) == JS_VALUE_GET_BOOL(b);
    case JS_TAG_NULL:
    case JS_TAG_UNDEFINED:
        return 1;
    case JS_TAG_FLOAT64:
        return JS_VALUE_GET_FLOAT64(a) == JS_VALUE_GET_FLOAT64(b);
    case JS_TAG_STRING:
        return JS_IsStrictEqual(ctx, a, b);
    default:
        /* Objects: reference equality */
        return JS_VALUE_GET_PTR(a) == JS_VALUE_GET_PTR(b);
    }
}

/* typeof helper */
static inline JSAtom js_typeof(JSContext *ctx, JSValue v) {
    switch (JS_VALUE_GET_TAG(v)) {
    case JS_TAG_INT:
    case JS_TAG_FLOAT64:
        return JS_ATOM_number;
    case JS_TAG_STRING:
        return JS_ATOM_string;
    case JS_TAG_BOOL:
        return JS_ATOM_boolean;
    case JS_TAG_UNDEFINED:
        return JS_ATOM_undefined;
    case JS_TAG_NULL:
        return JS_ATOM_object;  /* typeof null === "object" */
    case JS_TAG_SYMBOL:
        return JS_ATOM_symbol;
    case JS_TAG_BIG_INT:
        return JS_ATOM_bigint;
    default:
        if (JS_IsFunction(ctx, v))
            return JS_ATOM_function;
        return JS_ATOM_object;
    }
}

#endif /* FREEZE_RUNTIME_HELPERS_H */
