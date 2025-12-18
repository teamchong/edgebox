/**
 * EdgeBox Frozen Function Helpers
 *
 * This header defines the helper function registry that gets registered
 * at runtime via JS_SetRuntimeOpaque(). No QuickJS patches needed!
 *
 * Architecture:
 * 1. We define function pointer types for all helpers
 * 2. Create a registry struct with all function pointers
 * 3. Register the registry at runtime when creating JSRuntime
 * 4. Frozen code retrieves registry via JS_GetRuntimeOpaque()
 * 5. Frozen code calls helpers via function pointers
 */

#ifndef EDGEBOX_HELPERS_H
#define EDGEBOX_HELPERS_H

#include "quickjs.h"

/* Forward declarations for internal QuickJS types we need */
typedef struct JSVarRef JSVarRef;
typedef struct JSStackFrame JSStackFrame;

/**
 * EdgeBox Helper Function Registry
 * Registered via JS_SetRuntimeOpaque() - no patches needed!
 */
typedef struct EdgeBoxHelpers {
    /* Closure creation - needs JSVarRef** which is internal */
    JSValue (*js_closure)(JSContext *ctx, JSValue bfunc,
                         JSVarRef **cur_var_refs,
                         JSStackFrame *sf);

    /* For-in loop helpers */
    int (*js_frozen_for_in_start)(JSContext *ctx, JSValue *sp);
    int (*js_frozen_for_in_next)(JSContext *ctx, JSValue *sp);

    /* For-of loop helpers */
    int (*js_frozen_for_of_start)(JSContext *ctx, JSValue *sp, int is_async);
    int (*js_frozen_for_of_next)(JSContext *ctx, JSValue *sp, int offset);

    /* Closure variable access */
    JSValue (*js_frozen_get_var_ref)(JSContext *ctx, JSVarRef **var_refs, int idx);
    void (*js_frozen_set_var_ref)(JSContext *ctx, JSVarRef **var_refs, int idx, JSValue val);

    /* Home object registry for super */
    void (*JS_SetFrozenHomeObject)(JSContext *ctx, void *func_ptr, JSValue home_object);
    JSValue (*JS_GetFrozenHomeObject)(JSContext *ctx, void *func_ptr);

    /* import.meta support */
    JSValue (*JS_GetImportMetaCurrent)(JSContext *ctx);

    /* Arguments object */
    JSValue (*JS_NewMappedArgumentsSimple)(JSContext *ctx, int argc, JSValueConst *argv,
                                          int arg_count, int magic);

    /* Private field support */
    JSValue (*JS_NewPrivateSymbol)(JSContext *ctx, JSAtom atom);
    int (*JS_FrozenCheckBrand)(JSContext *ctx, JSValue obj, JSValue func);
    int (*JS_FrozenAddBrand)(JSContext *ctx, JSValue obj, JSValue func);
    int (*js_frozen_private_in)(JSContext *ctx, JSValue *sp);
    JSValue (*JS_FrozenGetPrivateField)(JSContext *ctx, JSValueConst obj, JSValueConst name);
    int (*JS_FrozenSetPrivateField)(JSContext *ctx, JSValueConst obj, JSValueConst name, JSValue val);
    int (*JS_FrozenDefinePrivateField)(JSContext *ctx, JSValueConst obj, JSValueConst name, JSValue val);
} EdgeBoxHelpers;

/**
 * Initialize and register EdgeBox helpers
 * Call this once when creating JSRuntime
 *
 * @param rt The QuickJS runtime
 * @return 0 on success, -1 on error
 */
int edgebox_init_helpers(JSRuntime *rt);

/**
 * Get the registered helpers from a context
 * Used by frozen code to access helpers
 *
 * @param ctx The QuickJS context
 * @return Pointer to helpers, or NULL if not initialized
 */
static inline EdgeBoxHelpers *edgebox_get_helpers(JSContext *ctx) {
    JSRuntime *rt = JS_GetRuntime(ctx);
    return (EdgeBoxHelpers *)JS_GetRuntimeOpaque(rt);
}

/**
 * Cleanup helpers when destroying runtime
 * Call before JS_FreeRuntime()
 *
 * @param rt The QuickJS runtime
 */
void edgebox_free_helpers(JSRuntime *rt);

#endif /* EDGEBOX_HELPERS_H */
