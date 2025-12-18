# Patch File Zero-Copy Optimization Opportunities

**Analysis Date**: 2025-01-18  
**Principle**: "Thin JS, Rich Zig, Zero Copy"

## Current Wrappers Analysis

### ✅ Good: True Zero-Copy Wrappers

These are perfect thin shims (no overhead):

```c
// Iterator helpers - direct passthrough
int js_frozen_for_in_start(JSContext *ctx, JSValue *sp) {
    return js_for_in_start(ctx, sp);  // ✅ Zero copy
}

int js_frozen_for_in_next(JSContext *ctx, JSValue *sp) {
    return js_for_in_next(ctx, sp);  // ✅ Zero copy
}
```

**Why good:**
- Single function call (inlined by compiler)
- No allocations
- No refcount changes
- Pointer passthrough only

---

### ⚠️ Issue #1: Unnecessary Duplication in get_var_ref

**Current implementation:**
```c
JSValue js_frozen_get_var_ref(JSContext *ctx, JSVarRef **var_refs, int idx)
{
    (void)ctx;
    if (!var_refs || !var_refs[idx]) return JS_UNDEFINED;
    return js_dup(*var_refs[idx]->pvalue);  // ❌ Always copies!
}
```

**Problem:**
- `js_dup()` increments refcount on EVERY read
- For readonly operations (like `x + y` where x is closure var), this is wasteful
- The value will be freed immediately after use anyway

**Optimization:**
```c
// Zero-copy version for readonly access
JSValue js_frozen_get_var_ref_borrow(JSContext *ctx, JSVarRef **var_refs, int idx)
{
    (void)ctx;
    if (!var_refs || !var_refs[idx]) return JS_UNDEFINED;
    return *var_refs[idx]->pvalue;  // ✅ Zero copy - borrowed reference
}

// Keep original for cases needing ownership
JSValue js_frozen_get_var_ref(JSContext *ctx, JSVarRef **var_refs, int idx)
{
    (void)ctx;
    if (!var_refs || !var_refs[idx]) return JS_UNDEFINED;
    return js_dup(*var_refs[idx]->pvalue);  // Owned reference
}
```

**Usage in codegen:**
```zig
// For immediate consumption (like arithmetic)
PUSH(js_frozen_get_var_ref_borrow(ctx, var_refs, idx));  // ✅ No refcount

// For storage (like put_loc)
locals[n] = js_frozen_get_var_ref(ctx, var_refs, idx);  // Need ownership
```

**Savings:**
- 1-2ns per closure var read (eliminates atomic increment)
- Significant for loops that read closure vars repeatedly

---

### ⚠️ Issue #2: O(n) Home Object Registry Lookup

**Current implementation:**
```c
#define FROZEN_HOME_OBJECT_REGISTRY_SIZE 256
static struct {
    void *func_ptr;
    JSValue home_object;
} frozen_home_object_registry[FROZEN_HOME_OBJECT_REGISTRY_SIZE];
static int frozen_home_object_count = 0;

JSValue JS_GetFrozenHomeObject(JSContext *ctx, void *func_ptr)
{
    (void)ctx;
    for (int i = 0; i < frozen_home_object_count; i++) {  // ❌ O(n) lookup!
        if (frozen_home_object_registry[i].func_ptr == func_ptr) {
            return frozen_home_object_registry[i].home_object;
        }
    }
    return JS_UNDEFINED;
}
```

**Problem:**
- Linear search through all registered functions
- Called on EVERY super method access
- With 256 functions, this is 128 comparisons on average

**Optimization 1: Hash Table**
```c
// Use simple modulo hash for fast lookup
#define HOME_OBJECT_HASH_SIZE 512
#define HOME_OBJECT_HASH(ptr) (((uintptr_t)(ptr) >> 4) % HOME_OBJECT_HASH_SIZE)

static struct {
    void *func_ptr;
    JSValue home_object;
    int next;  // Collision chain
} home_object_hash[HOME_OBJECT_HASH_SIZE];

JSValue JS_GetFrozenHomeObject(JSContext *ctx, void *func_ptr)
{
    (void)ctx;
    int idx = HOME_OBJECT_HASH(func_ptr);
    while (idx != -1) {
        if (home_object_hash[idx].func_ptr == func_ptr) {
            return home_object_hash[idx].home_object;  // ✅ O(1) average
        }
        idx = home_object_hash[idx].next;
    }
    return JS_UNDEFINED;
}
```

**Optimization 2: Direct Storage (Best)**
```c
// Since we generate the C functions, we can just use a static variable!
// In codegen_ssa.zig, generate:

static JSValue __frozen_methodName_home_object = {0};

// No registry needed - direct access!
JSValue get_home() {
    return __frozen_methodName_home_object;  // ✅ Zero cost!
}
```

**Why Option 2 is best:**
- Zero runtime lookup
- No hash collisions
- Compiler can inline
- Same pattern as `___frozen_X_this_func`

---

### ⚠️ Issue #3: Constant Pool Access via Function Call

**Current:**
```c
JSValue* JS_GetFunctionConstantPool(JSContext *ctx, JSValueConst func_obj, int *pcount)
{
    JSObject *p;
    JSFunctionBytecode *b;
    
    if (JS_VALUE_GET_TAG(func_obj) != JS_TAG_OBJECT)
        return NULL;
    
    p = JS_VALUE_GET_OBJ(func_obj);  // Pointer dereference
    if (!js_class_has_bytecode(p->class_id))
        return NULL;
    
    b = p->u.func.function_bytecode;  // Another dereference
    if (!b || !b->cpool)
        return NULL;
    
    if (pcount)
        *pcount = b->cpool_count;
    return b->cpool;  // Finally return the pointer
}
```

**Problem:**
- Called during function init (not hot path, but still...)
- Multiple pointer chases
- Branch predictions

**Optimization:**
Since frozen functions are generated at compile time, we can:

```c
// In codegen: Generate this per-function
static JSValue *___frozen_methodName_cpool = NULL;
static int ___frozen_methodName_cpool_count = 0;

// In init function:
___frozen_methodName_cpool = original_func_bytecode->cpool;  // ✅ Direct assignment
___frozen_methodName_cpool_count = original_func_bytecode->cpool_count;

// Usage in function body:
JSValue func = ___frozen_methodName_cpool[idx];  // ✅ Direct array access
```

**Already done!** This is exactly how it works currently. Good!

---

## Summary of Optimizations

| Issue | Current | Optimized | Savings |
|-------|---------|-----------|---------|
| **get_var_ref dup** | Always `js_dup()` | Borrow for readonly | 1-2ns per read |
| **Home object lookup** | O(n) linear search | Static per-function | ~50ns → 0ns |
| **Constant pool** | ✅ Already optimized | N/A | N/A |

## Implementation Priority

1. **High**: Home object direct storage (easy win, big impact)
2. **Medium**: get_var_ref_borrow (moderate complexity, good savings)
3. **Low**: Other micro-optimizations

## Estimated Total Impact

For a typical frozen closure function that:
- Reads 2 closure vars per iteration
- Uses super methods (home object lookup)
- Runs 1000 iterations

**Savings:** ~50-100μs total (5-10% improvement in closure-heavy code)
