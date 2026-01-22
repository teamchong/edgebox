/*
 * Runtime helpers for frozen functions
 * These are thin wrappers around QuickJS internal functions
 */

#ifndef FREEZE_RUNTIME_HELPERS_H
#define FREEZE_RUNTIME_HELPERS_H

#include "quickjs.h"
#include "cutils.h"

/* Int to float conversion - used by Zig WASM32 to bypass LLVM codegen bug */
double frozen_int_to_float(int32_t val) {
    return (double)val;
}

/* Convert CompressedValue (as two u32s) to JSValue - bypasses Zig LLVM WASM32 u64 return bug
 * NaN-boxing format:
 *   Float: !NaN pattern in hi (bit 51-62 not all 1s)
 *   Int: QNAN_HI | TAG_INT_HI (0x7FF90000) in hi, value in lo
 *   Ptr: QNAN_HI | TAG_PTR_HI (0x7FFD0000) in hi, ptr in lo
 *   Str: QNAN_HI | TAG_STR_HI (0x7FFF0000) in hi, ptr in lo
 *   Bool: QNAN_HI | TAG_BOOL_HI (0x7FF10000) in hi, 0/1 in lo
 *   Null: QNAN_HI | TAG_NULL_HI (0x7FF20000) in hi, 0 in lo
 *   Undef: QNAN_HI | TAG_UNDEF_HI (0x7FF30000) in hi, 0 in lo
 */
JSValue frozen_cv_to_jsvalue(JSContext *ctx, uint32_t lo, uint32_t hi) {
    #define QNAN_HI     0x7FF80000u
    #define TAG_MASK_HI 0xFFFF0000u
    #define TAG_INT_HI  0x00010000u
    #define TAG_BOOL_HI 0x00010000u  /* Same as int for simplicity */
    #define TAG_NULL_HI 0x00020000u
    #define TAG_UNDEF_HI 0x00030000u
    #define TAG_PTR_HI  0x00050000u
    #define TAG_STR_HI  0x00070000u

    /* Check if it's a float (not a NaN-boxed special value) */
    int is_nan = (hi & 0x7FF00000u) == 0x7FF00000u;

    if (!is_nan) {
        /* It's a float - reconstruct f64 from lo/hi */
        union {
            uint32_t parts[2];
            double d;
        } u;
        u.parts[0] = lo;
        u.parts[1] = hi;
        return JS_NewFloat64(ctx, u.d);
    }

    uint32_t tag_bits = hi & TAG_MASK_HI;

    /* Integer: QNAN_HI | TAG_INT_HI = 0x7FF90000 */
    if (tag_bits == (QNAN_HI | TAG_INT_HI)) {
        return JS_MKVAL(JS_TAG_INT, (int32_t)lo);
    }

    /* Object/Function pointer: QNAN_HI | TAG_PTR_HI = 0x7FFD0000 */
    if (tag_bits == (QNAN_HI | TAG_PTR_HI)) {
        JSValue result;
        result.u.ptr = (void*)(uintptr_t)lo;
        result.tag = JS_TAG_OBJECT;
        return result;
    }

    /* String pointer: QNAN_HI | TAG_STR_HI = 0x7FFF0000 */
    if (tag_bits == (QNAN_HI | TAG_STR_HI)) {
        JSValue result;
        result.u.ptr = (void*)(uintptr_t)lo;
        result.tag = JS_TAG_STRING;
        return result;
    }

    /* Special values */
    if (hi == (QNAN_HI | TAG_UNDEF_HI) && lo == 0) return JS_UNDEFINED;
    if (hi == (QNAN_HI | TAG_NULL_HI) && lo == 0) return JS_NULL;
    if (hi == (QNAN_HI | 0x00010000u) && lo == 1) return JS_TRUE;
    if (hi == (QNAN_HI | 0x00010000u) && lo == 0) return JS_FALSE;

    /* Unknown - return undefined */
    return JS_UNDEFINED;

    #undef QNAN_HI
    #undef TAG_MASK_HI
    #undef TAG_INT_HI
    #undef TAG_BOOL_HI
    #undef TAG_NULL_HI
    #undef TAG_UNDEF_HI
    #undef TAG_PTR_HI
    #undef TAG_STR_HI
}

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
