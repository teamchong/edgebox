# Block-Level Fallback: Locals vs Closure Variables

**Date**: 2025-01-18
**Key Finding**: Block-level fallback requires understanding how locals and closure variables are stored

## The Problem

When implementing block-level fallback (executing clean blocks in frozen C, contaminated blocks in interpreter), we need to ensure **local variables are preserved** across transitions.

## Current Implementation

### Partial Freeze Already Works

Partial freeze is already implemented and working:

```bash
# testPartial: 3/12 blocks clean (PARTIAL FROZEN)
zig-out/bin/edgebox zig-out/bin/tmp/test_partial/test_partial.wasm
# Output: 45 ✅
```

**How it works:**
- Clean blocks execute in frozen C
- When hitting contaminated block, jumps to `interpreter_fallback:`
- Calls `frozen_fallback_call(ctx, "testPartial", this_val, argc, argv)`
- This re-executes the **ENTIRE function** in interpreter (function-level fallback)

## Key Distinction: Locals vs Closures

### Local Variables (C Stack)

**Example: `outer` function**
```javascript
function outer(x) {
    let count = 0;  // Local variable
    for (let i = 0; i < x; i++) {
        count += i;
    }
    try {
        if (count > 50) throw new Error("big");
    } catch (e) {
        count = 0;  // Modifies local
    }
    return function inner() {
        return count;  // Closure: captures count
    };
}
```

**Generated C code:**
```c
static JSValue __frozen_outer(JSContext *ctx, JSValueConst this_val,
                              int argc, JSValueConst *argv) {
    JSValue locals[3];  // ← locals[0] = count
    for (int i = 0; i < 3; i++) locals[i] = JS_UNDEFINED;

    // Block 0-1: Modify locals[0] directly
    locals[0] = JS_MKVAL(JS_TAG_INT, 0);

    // Block 2: Contaminated (try-catch)
    goto interpreter_fallback;  // ← PROBLEM: locals[] lost!
}
```

**Problem**:
- `locals[]` is a C stack array
- When we call `frozen_fallback_call()`, the C stack unwinds
- Local variables are lost
- Can't resume frozen execution after interpreter block

### Closure Variables (Heap-Allocated, Passed via argv)

**Example: `inner` function**
```javascript
return function inner() {
    return count;  // Accesses outer's count
};
```

**Generated C code:**
```c
static JSValue __frozen_inner(JSContext *ctx, JSValueConst this_val,
                              int argc, JSValueConst *argv) {
    // argv[0] = closure vars object { count: ... }
    PUSH(FROZEN_DUP(ctx, JS_GetPropertyUint32(ctx, argv[0], 0)));
    return POP();
}
```

**Why it works:**
- Closure vars stored in JS heap object
- Passed via `argv[0]` (pointer)
- Pointer stays valid across function boundaries
- Already works for block-level fallback!

## Closure Manifest

The compiler generates a manifest showing which functions have closures:

```c
/* CLOSURE_MANIFEST_BEGIN
{"functions":[
  {"name":"tick","closureVars":[{"n":"count","c":false},{"n":"tick","c":false}]},
  {"name":"inner","closureVars":[{"n":"count","c":false}]}
]}
CLOSURE_MANIFEST_END */
```

**Legend:**
- `"n"`: Variable name
- `"c"`: Is callable (function reference)

## Solution for Block-Level Fallback

To enable true block-level fallback (frozen → interpreter → frozen), we need to:

### Option 1: Pass Locals Pointer to Interpreter

Add `JS_ExecuteBytecodeBlock` API to QuickJS that accepts locals array:

```c
JSValue JS_ExecuteBytecodeBlock(JSContext *ctx,
                                JSValueConst func_obj,
                                uint32_t start_pc,
                                uint32_t end_pc,
                                JSValue *stack_in_out,  // ✅ Zero copy
                                uint32_t *sp_in_out,    // ✅ Zero copy
                                JSValue *locals,        // ✅ Zero copy - frozen's locals!
                                uint32_t *next_pc_out);
```

**How it works:**
1. Frozen code executes clean blocks with `locals[]` array
2. On contaminated block, calls `JS_ExecuteBytecodeBlock` with pointer to `locals[]`
3. Interpreter modifies `locals[]` in-place (zero-copy!)
4. Returns next PC
5. Frozen code resumes with updated `locals[]`

**Example:**
```c
static JSValue __frozen_outer(...) {
    JSValue locals[3];

    // Block 0-1: Clean, execute in frozen C
    locals[0] = JS_MKVAL(JS_TAG_INT, 45);

    // Block 2: Contaminated (try-catch)
    {
        uint32_t next_pc;
        uint32_t resume_sp = sp;
        JSValue result = JS_ExecuteBytecodeBlock(ctx,
            __outer_original_func,
            BLOCK_2_START_PC,
            BLOCK_2_END_PC,
            stack,      // ✅ Same stack
            &resume_sp, // ✅ Updated sp
            locals,     // ✅ Same locals array!
            &next_pc);

        if (JS_IsException(result)) return result;
        sp = resume_sp;
        goto *block_jumps[next_pc];  // Resume frozen
    }

    // Block 3: Clean, execute in frozen C with updated locals
    return create_closure_with_count(locals[0]);
}
```

### Option 2: Convert Locals to Heap Before Fallback (Not Recommended)

Alternative approach would be to convert `locals[]` to a JS object before calling interpreter:

```c
// Convert locals to heap object
JSValue locals_obj = JS_NewObject(ctx);
for (int i = 0; i < num_locals; i++) {
    JS_SetPropertyUint32(ctx, locals_obj, i, locals[i]);
}

// Call interpreter
JSValue result = frozen_fallback_call(...);

// Convert back
for (int i = 0; i < num_locals; i++) {
    locals[i] = JS_GetPropertyUint32(ctx, locals_obj, i);
}
JS_FreeValue(ctx, locals_obj);
```

**Why not recommended:**
- Allocates JS object (not zero-copy)
- Requires copying all locals twice
- Slower than direct pointer pass
- Breaks "thin JS, rich Zig" principle

## Current Status

✅ **Working:**
- Closures are frozen (18x speedup)
- Partial freeze generates code successfully
- Function-level fallback works (re-executes entire function)

❌ **Not Yet Implemented:**
- Block-level fallback (contaminated block only)
- `JS_ExecuteBytecodeBlock` API in QuickJS patch
- Zero-copy locals handoff

## Performance Impact

**Current (function-level fallback):**
- Function with 3/12 clean blocks = 0x speedup (full interpreter)
- Why? Entire function runs in interpreter even though 25% is clean

**Proposed (block-level fallback):**
- Function with 3/12 clean blocks = ~5x speedup
- 25% frozen (18x) + 75% interpreted (1x) = 5.25x overall
- Even better for functions with higher clean block percentage

**Example: testPartial (3/12 clean = 25%)**
- Before: 1x (100% interpreter)
- After: 0.25 × 18 + 0.75 × 1 = **5.25x speedup**

**Example: typical function (80% clean)**
- Before: 1x (100% interpreter)
- After: 0.80 × 18 + 0.20 × 1 = **14.6x speedup**

## Next Steps

1. **Design `JS_ExecuteBytecodeBlock` API** in QuickJS patch
2. **Generate block PC mappings** in `codegen_ssa.zig`
3. **Emit block transition code** for contaminated blocks
4. **Test with try-catch example** (testPartial)
5. **Benchmark improvement** (expect 5-15x for partially freezable functions)

## Why Closures Don't Block Us

The key insight: **Closures already work for block-level fallback!**

- Closure vars passed via `argv[0]` (heap pointer)
- Pointer valid across frozen/interpreter boundaries
- No special handling needed for closures

**The blocker is locals, not closures.**

We solved closures in the previous session (fclosure8/set_name). Now we need to solve locals for block-level fallback.
