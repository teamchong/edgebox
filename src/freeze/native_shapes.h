/*
 * Native Shape Access - Zero-Overhead Property Access for Known Shapes
 *
 * This header provides fast property access for objects with known shapes
 * (like TypeScript AST nodes). Instead of going through QuickJS's hash table
 * lookup, we access native memory directly.
 *
 * Usage in frozen functions:
 *   1. Check if object has native representation: native_node_lookup(addr)
 *   2. If found, access properties directly: native_get_kind(node)
 *   3. If not found, fall back to QuickJS: JS_GetPropertyStr(ctx, obj, "kind")
 *
 * Performance:
 *   - Native access: ~1 cycle
 *   - QuickJS access: ~35 cycles
 */

#ifndef NATIVE_SHAPES_H
#define NATIVE_SHAPES_H

#include <stdint.h>
#include <stdio.h>  /* for debug fprintf */
#include "quickjs.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Native AstNode structure - mirrors TypeScript AST node */
typedef struct NativeAstNode {
    int32_t kind;           /* SyntaxKind enum */
    int32_t flags;          /* NodeFlags */
    int32_t pos;            /* Start position */
    int32_t end;            /* End position */
    struct NativeAstNode *parent;  /* Parent node */
    uint64_t js_value;      /* Original JSValue bits */
    int32_t modifier_flags_cache;
    int32_t transform_flags;
} NativeAstNode;

/* Registry functions - call from frozen_init */
void native_registry_init(void);
void native_registry_deinit(void);

/* Register a native node (call when createNode is intercepted) */
NativeAstNode* native_node_register(uint64_t js_addr, int32_t kind, int32_t flags, int32_t pos, int32_t end);

/* 32-bit version for WASM ABI compatibility */
NativeAstNode* native_node_register32(uint32_t js_addr32, int32_t kind, int32_t flags, int32_t pos, int32_t end);

/* Fast lookup - returns NULL if not a known native node */
NativeAstNode* native_node_lookup(uint64_t js_addr);

/* Get count of registered nodes (for debugging) */
int native_registry_count(void);

/* Fast property accessors */
int32_t native_get_kind(NativeAstNode *node);
int32_t native_get_flags(NativeAstNode *node);
int32_t native_get_pos(NativeAstNode *node);
int32_t native_get_end(NativeAstNode *node);
NativeAstNode* native_get_parent(NativeAstNode *node);

/* ============================================================================
 * Inline fast-path macros for generated frozen code
 * ============================================================================ */

/* Extract JSValue address for registry lookup */
static inline uint64_t jsvalue_to_addr(JSValue val) {
    /* For objects, use the object pointer as the key.
     * JSValue layout varies by platform:
     *   - struct-based (WASM): { JSValueUnion u; int64_t tag; }
     *   - uint64_t NaN-boxed: high 32 = tag, low 32 = value/ptr
     *   - pointer-based: tagged pointer
     *
     * Use JS_VALUE_GET_PTR to extract the pointer portably.
     */
    return (uint64_t)(uintptr_t)JS_VALUE_GET_PTR(val);
}

/* Convert native int32 to JSValue */
static inline JSValue native_int_to_jsvalue(int32_t val) {
    return JS_NewInt32(NULL, val);  /* NULL ctx is safe for NewInt32 */
}

/* Debug: Log lookup address */
extern void native_debug_log_lookup(uint64_t addr, int found);

/* Debug: Get last lookup info */
extern uint64_t native_debug_get_last_lookup(void);
extern int native_debug_get_found(void);
extern int native_debug_get_call_count(void);

/* Debug: Get registration info */
extern uint64_t native_debug_get_registered_addr(void);
extern int native_debug_get_register_success(void);

/* Debug: Get 32-bit registration info */
extern uint32_t native_debug_get_register32_addr(void);
extern int native_debug_get_register32_called(void);

/*
 * NATIVE_GET_KIND - Fast path for node.kind access
 * Falls back to QuickJS if not a native node
 */
#define NATIVE_GET_KIND(ctx, obj) ({ \
    JSValue _result; \
    uint64_t _addr = jsvalue_to_addr(obj); \
    NativeAstNode *_native = native_node_lookup(_addr); \
    native_debug_log_lookup(_addr, _native != NULL); \
    if (_native) { \
        _result = JS_NewInt32(ctx, _native->kind); \
    } else { \
        _result = JS_GetPropertyStr(ctx, obj, "kind"); \
    } \
    _result; \
})

/*
 * NATIVE_GET_FLAGS - Fast path for node.flags access
 */
#define NATIVE_GET_FLAGS(ctx, obj) ({ \
    JSValue _result; \
    NativeAstNode *_native = native_node_lookup(jsvalue_to_addr(obj)); \
    if (_native) { \
        _result = JS_NewInt32(ctx, _native->flags); \
    } else { \
        _result = JS_GetPropertyStr(ctx, obj, "flags"); \
    } \
    _result; \
})

/*
 * NATIVE_GET_POS - Fast path for node.pos access
 */
#define NATIVE_GET_POS(ctx, obj) ({ \
    JSValue _result; \
    NativeAstNode *_native = native_node_lookup(jsvalue_to_addr(obj)); \
    if (_native) { \
        _result = JS_NewInt32(ctx, _native->pos); \
    } else { \
        _result = JS_GetPropertyStr(ctx, obj, "pos"); \
    } \
    _result; \
})

/*
 * NATIVE_GET_END - Fast path for node.end access
 */
#define NATIVE_GET_END(ctx, obj) ({ \
    JSValue _result; \
    NativeAstNode *_native = native_node_lookup(jsvalue_to_addr(obj)); \
    if (_native) { \
        _result = JS_NewInt32(ctx, _native->end); \
    } else { \
        _result = JS_GetPropertyStr(ctx, obj, "end"); \
    } \
    _result; \
})

/*
 * NATIVE_GET_PARENT - Fast path for node.parent access
 */
#define NATIVE_GET_PARENT(ctx, obj) ({ \
    JSValue _result; \
    NativeAstNode *_native = native_node_lookup(jsvalue_to_addr(obj)); \
    if (_native && _native->parent) { \
        /* Return the original JSValue of the parent */ \
        union { JSValue v; uint64_t u; } _u; \
        _u.u = _native->parent->js_value; \
        _result = JS_DupValue(ctx, _u.v); \
    } else { \
        _result = JS_GetPropertyStr(ctx, obj, "parent"); \
    } \
    _result; \
})

/*
 * Check if a property name is optimizable with native access
 * Used by code generator to decide whether to use NATIVE_GET_* macros
 */
static inline int is_native_optimizable_property(const char *name) {
    return (name[0] == 'k' && name[1] == 'i' && name[2] == 'n' && name[3] == 'd' && name[4] == '\0') ||
           (name[0] == 'f' && name[1] == 'l' && name[2] == 'a' && name[3] == 'g' && name[4] == 's' && name[5] == '\0') ||
           (name[0] == 'p' && name[1] == 'o' && name[2] == 's' && name[3] == '\0') ||
           (name[0] == 'e' && name[1] == 'n' && name[2] == 'd' && name[3] == '\0') ||
           (name[0] == 'p' && name[1] == 'a' && name[2] == 'r' && name[3] == 'e' && name[4] == 'n' && name[5] == 't' && name[6] == '\0');
}

#ifdef __cplusplus
}
#endif

#endif /* NATIVE_SHAPES_H */
