# Block-Level Fallback: Simple Heap-Based Approach

**Date**: 2025-01-18
**Insight**: Instead of complex `JS_ExecuteBytecodeBlock` API, use heap storage for locals/stack

## The Problem (Revisited)

When frozen code hits a contaminated block:
- Locals stored in C stack array: `JSValue locals[3]`
- Stack stored in C array: `JSValue stack[256]`
- Calling `frozen_fallback_call()` loses this state

## Original Solution (Complex)

Add `JS_ExecuteBytecodeBlock` API to execute specific PC range with locals/stack pointers.

**Problems:**
- Requires deep QuickJS internals knowledge
- Must modify bytecode interpreter loop
- Complex to implement correctly
- High risk of bugs

## New Solution (Simple): Heap Storage

**Key insight**: Convert locals/stack to JS heap objects before fallback!

### Step 1: Add Helper Functions

```c
// In frozen_runtime.c

/**
 * Store locals array in a JS object for fallback preservation
 */
JSValue frozen_locals_to_object(JSContext *ctx, JSValue *locals, int num_locals) {
    JSValue obj = JS_NewArray(ctx);
    for (int i = 0; i < num_locals; i++) {
        JS_SetPropertyUint32(ctx, obj, i, JS_DupValue(ctx, locals[i]));
    }
    return obj;
}

/**
 * Restore locals array from JS object after fallback
 */
void frozen_object_to_locals(JSContext *ctx, JSValue obj, JSValue *locals, int num_locals) {
    for (int i = 0; i < num_locals; i++) {
        JSValue val = JS_GetPropertyUint32(ctx, obj, i);
        JS_FreeValue(ctx, locals[i]);
        locals[i] = val; /* Takes ownership */
    }
}

/**
 * Block-level fallback: execute single contaminated block
 * Returns updated locals as JS object
 */
JSValue frozen_block_fallback(JSContext *ctx, const char *func_name,
                               JSValue this_val, int argc, JSValue *argv,
                               JSValue locals_obj, int block_id) {
    /* Create extended argv with locals */
    JSValue *extended_argv = js_malloc(ctx, sizeof(JSValue) * (argc + 2));
    if (!extended_argv) return JS_EXCEPTION;

    /* Copy original args */
    for (int i = 0; i < argc; i++) {
        extended_argv[i] = JS_DupValue(ctx, argv[i]);
    }

    /* Append locals and block_id */
    extended_argv[argc] = JS_DupValue(ctx, locals_obj);
    extended_argv[argc + 1] = JS_NewInt32(ctx, block_id);

    /* Call __fallback_{func_name}(args..., locals, block_id) */
    char fallback_name[256];
    snprintf(fallback_name, sizeof(fallback_name), "__fallback_%s", func_name);

    JSValue global = JS_GetGlobalObject(ctx);
    JSValue fallback_func = JS_GetPropertyStr(ctx, global, fallback_name);
    JS_FreeValue(ctx, global);

    if (!JS_IsFunction(ctx, fallback_func)) {
        js_free(ctx, extended_argv);
        JS_FreeValue(ctx, fallback_func);
        return JS_ThrowReferenceError(ctx, "Block fallback '%s' not found", fallback_name);
    }

    /* Call fallback */
    JSValue result = JS_Call(ctx, fallback_func, this_val, argc + 2, extended_argv);

    /* Cleanup */
    for (int i = 0; i < argc + 2; i++) {
        JS_FreeValue(ctx, extended_argv[i]);
    }
    js_free(ctx, extended_argv);
    JS_FreeValue(ctx, fallback_func);

    return result; /* Returns updated locals object */
}
```

### Step 2: Generate Fallback JS Functions

In `codegen_ssa.zig`, for partial freeze functions, generate a JS fallback:

```javascript
// Generated JS fallback for block execution
function __fallback_testPartial(n, locals, block_id) {
    // Restore locals from array
    let sum = locals[0];
    let i = locals[1];

    // Execute the requested block
    if (block_id === 2) {
        // Block 2: try-catch
        try {
            if (sum > 100) throw new Error("too big");
            return sum;
        } catch (e) {
            return -1;
        }
    }

    // Return updated locals
    return [sum, i];
}
```

### Step 3: Update Frozen C Code Generation

```c
static JSValue __frozen_testPartial(JSContext *ctx, JSValueConst this_val,
                                   int argc, JSValueConst *argv) {
    JSValue locals[2];
    locals[0] = JS_UNDEFINED;
    locals[1] = JS_UNDEFINED;

    JSValue stack[256];
    int sp = 0;

    // Block 0-1: Clean, execute in frozen C
    locals[0] = JS_MKVAL(JS_TAG_INT, 0);  // sum = 0
    locals[1] = JS_MKVAL(JS_TAG_INT, 0);  // i = 0

    // ... loop code ...
    // After loop: locals[0] = 45

    // Block 2: Contaminated (try-catch)
    {
        /* Convert locals to heap object */
        JSValue locals_obj = frozen_locals_to_object(ctx, locals, 2);

        /* Call block fallback */
        JSValue result = frozen_block_fallback(ctx, "testPartial",
                                               this_val, argc, argv,
                                               locals_obj, 2);
        JS_FreeValue(ctx, locals_obj);

        if (JS_IsException(result)) {
            FROZEN_EXIT_STACK();
            return result;
        }

        /* If result is array, it's updated locals - restore them */
        if (JS_IsArray(ctx, result)) {
            frozen_object_to_locals(ctx, result, locals, 2);
            JS_FreeValue(ctx, result);
            /* Continue to next block */
        } else {
            /* Result is the return value - return it */
            FROZEN_EXIT_STACK();
            return result;
        }
    }

    // Block 3: Clean, continue with updated locals
    PUSH(locals[0]);
    FROZEN_EXIT_STACK();
    return POP();
}
```

## Benefits of This Approach

### 1. Simpler Implementation
- No QuickJS internals modification needed
- Uses existing JS function calls
- Clear separation: frozen (C) vs interpreter (JS)

### 2. Debugging Friendly
- Fallback is plain JS - easy to inspect
- Can add console.log in fallback functions
- Stack traces work normally

### 3. Zero-Copy for Closures
- Closures already passed via argv (still zero-copy!)
- Only locals need heap conversion (small overhead)
- Typical functions have 1-5 locals (cheap to copy)

### 4. Flexible
- Can add logging/tracing to fallback
- Can instrument contaminated blocks
- Easy to test fallback behavior

## Performance Analysis

**Overhead per block transition:**
1. Create locals array: ~50ns (2-5 locals typical)
2. Call JS function: ~20ns (optimized bytecode)
3. Restore locals array: ~50ns

**Total: ~120ns per block transition**

**Compared to function-level fallback:**
- Function-level: Entire function in interpreter (100% slow)
- Block-level: Clean blocks in C (18x fast) + transition overhead

**Example: testPartial (3/12 clean blocks, 25% clean)**

**Before (function-level):**
- 100% interpreter = 1000ns total

**After (block-level):**
- 75% interpreter blocks = 750ns
- 25% frozen blocks = 15ns (18x faster)
- 1 transition = 120ns
- Total = 750 + 15 + 120 = **885ns**

Wait, that's only 1.13x faster? 🤔

**Re-calculation for larger function (80% clean):**
- 20% interpreter = 200ns
- 80% frozen = 44ns (18x faster)
- 1 transition = 120ns
- Total = 200 + 44 + 120 = **364ns vs 1000ns = 2.7x faster**

**The issue**: Transition overhead dominates for functions with small clean blocks!

## Optimization: Batch Clean Blocks

**Better approach**: Only transition when FORCED to, not per-block.

```c
// Execute blocks 0-1 in frozen (no transition)
// Only transition at block 2 (contaminated)
// If block 3 is clean and simple, let interpreter handle it (avoid transition)
```

**Decision rule:**
- If remaining work is small, stay in interpreter
- Only resume frozen if significant work remains

This reduces transitions and improves performance for partially-contaminated functions.

## Implementation Priority

1. ✅ **Simple version**: Function-level fallback (already works)
2. 🔄 **This approach**: Heap-based block fallback (for complex functions)
3. ⏭️ **Future**: Native `JS_ExecuteBytecodeBlock` (if heap overhead is measurable)

## Next Steps

1. Implement `frozen_locals_to_object` and `frozen_block_fallback` in `frozen_runtime.c`
2. Update `codegen_ssa.zig` to generate JS fallback functions
3. Emit block transition code with locals conversion
4. Test with testPartial
5. Benchmark actual overhead

## When to Use Block-Level Fallback

**Good candidates:**
- Large functions (>100 instructions)
- High clean block percentage (>70%)
- Few transitions (1-2 contaminated regions)

**Not worth it:**
- Small functions (<50 instructions)
- Many small contaminated blocks (transition overhead)
- Low clean percentage (<50%)

The compiler should decide automatically based on cost/benefit analysis!
