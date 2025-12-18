# Block-Level Zero-Copy Fallback Design

**Date**: 2025-01-18  
**Goal**: Break function-level freezing boundary → block-level hybrid execution  
**Principle**: "Thin JS, Rich Zig, Zero Copy"

## Current State: Function-Level Fallback ❌

**When a function has ANY contaminated block:**
```
Contaminated Block Found → Entire function runs in interpreter
```

**Example:**
```javascript
function test(n) {
    let sum = 0;              // Block 0: ✅ Can freeze
    for (let i = 0; i < n; i++) {   // Block 1: ✅ Can freeze  
        sum += i;
    }
    try {                      // Block 2: ❌ Contaminated (try-catch)
        return eval("sum");
    } catch (e) {
        return 0;
    }
}
```

**Current behavior**: Entire function runs in interpreter (even clean blocks 0 and 1!)

---

## Proposed: Block-Level Zero-Copy Fallback ✅

**Execute clean blocks in frozen C, contaminated blocks in interpreter:**
```
Block 0 (frozen) → Block 1 (frozen) → Block 2 (interpreter) → Return
```

**Key requirement**: Zero-copy stack/locals handoff at block boundaries.

---

## Design: Stack State Preservation

### Problem: Different Stack Layouts

**Frozen C code:**
```c
JSValue stack[256];  // C array
int sp = 0;          // Stack pointer
JSValue locals[8];   // Local variables
```

**Interpreter:**
```c
// QuickJS internal structures (not directly accessible)
JSValue *stack;      // Managed by interpreter
uint32_t sp;         // Internal stack pointer
```

### Solution: Shared Stack Structure

**Idea**: Use QuickJS's native stack format for frozen code too!

```c
// Instead of our own stack, use QuickJS-compatible layout
typedef struct {
    JSValue *stack;       // ← Points to QuickJS stack
    uint32_t sp;          // ← QuickJS sp
    JSValue *locals;      // ← Points to QuickJS locals
    uint32_t pc;          // ← Program counter for resume
} FrozenContext;

// Frozen blocks use this structure (zero-copy!)
JSValue frozen_block_0(JSContext *ctx, FrozenContext *fc) {
    JSValue *stack = fc->stack;   // ✅ Zero copy - same memory!
    int sp = fc->sp;
    JSValue *locals = fc->locals;
    
    // ... frozen code ...
    
    fc->sp = sp;  // Update for next block
    return JS_UNDEFINED;
}
```

---

## Implementation Strategy

### Step 1: Add Block-Level Bytecode Execution API

Add to QuickJS patch:
```c
/* Execute bytecode starting at PC until jump/return/contaminated block */
JSValue JS_ExecuteBytecodeBlock(JSContext *ctx, 
                                 JSValueConst func_obj,
                                 uint32_t start_pc,
                                 uint32_t end_pc,
                                 JSValue *stack_in_out,
                                 uint32_t *sp_in_out,
                                 JSValue *locals);
```

**Why this is thin/zero-copy:**
- Pointers to existing stack (no copy)
- Returns updated sp (no allocation)
- Resumes exactly where frozen left off

### Step 2: Generate Block Transition Code

In `codegen_ssa.zig`, for contaminated blocks:

**Current (function-level fallback):**
```c
block_2: /* contaminated: catch */
    goto interpreter_fallback;  // ❌ Calls entire function

interpreter_fallback:
    return frozen_fallback_call(ctx, "test", this_val, argc, argv);
```

**Proposed (block-level fallback):**
```c
block_2: /* contaminated: catch */
    // ✅ Execute JUST this block in interpreter, then resume frozen
    {
        uint32_t resume_sp = sp;
        JSValue result = JS_ExecuteBytecodeBlock(ctx, 
            __test_original_func,  // Original bytecode function
            BLOCK_2_START_PC,      // Start at block 2
            BLOCK_2_END_PC,        // End at block 2
            stack,                 // ✅ Zero copy - same stack!
            &resume_sp,            // Update sp
            locals);               // ✅ Zero copy - same locals!
        
        if (JS_IsException(result)) return result;
        sp = resume_sp;
        
        // Jump to next block (determined by bytecode execution)
        int next_block_id = __test_block_map[BLOCK_2_END_PC];
        goto *block_jumps[next_block_id];  // Computed goto
    }
```

### Step 3: Generate Block PC Mappings

```c
// Generated at compile time
static const uint32_t __test_block_pcs[] = {
    0,    // Block 0 starts at PC 0
    15,   // Block 1 starts at PC 15
    42,   // Block 2 starts at PC 42 (contaminated!)
    67,   // Block 3 starts at PC 67
};

static const int __test_block_map[256];  // PC → block_id lookup
```

---

## Benefits

### 1. Performance
- **Clean blocks**: Native C speed (18x faster)
- **Contaminated blocks**: Interpreter speed (baseline)
- **Overall**: 5-10x improvement even with some contamination

**Example**: Function with 80% clean blocks, 20% contaminated
- Current: 100% interpreter = 1x speed
- Block-level: 80% frozen + 20% interpreted = ~15x speed

### 2. Zero Copy
- No stack copying at transitions
- No data serialization
- Direct pointer handoff
- Locals shared via pointer

### 3. Gradual Degradation
- More freezable code = better performance
- Functions don't become "all or nothing"
- Graceful performance curve

---

## Challenges & Solutions

### Challenge 1: Block Exit Determination

**Problem**: How does frozen code know which block to jump to next?

**Solution 1**: Bytecode execution returns next PC
```c
uint32_t next_pc;
JSValue result = JS_ExecuteBytecodeBlock(..., &next_pc);
int next_block = __test_block_map[next_pc];
```

**Solution 2**: Interpreter sets global
```c
// In interpreter, after executing block
ctx->frozen_next_block_id = 3;  // Jump to block 3
```

### Challenge 2: Stack Height Mismatch

**Problem**: Interpreter might leave different number of values on stack.

**Solution**: Block entry expects specific stack height
```c
// Each block knows its expected stack height
static const int __test_block_entry_sp[] = {0, 0, 1, 0};

// On resume:
if (sp != __test_block_entry_sp[next_block]) {
    // Stack corruption - bail to interpreter for rest
    goto full_interpreter_fallback;
}
```

### Challenge 3: Local Variable Modifications

**Problem**: Interpreter might modify locals in unexpected ways.

**CRITICAL INSIGHT**: Locals vs Closure Variables are handled differently!

**Local variables** (in `outer`):
- `let count = 0;` is stored in `locals[0]`
- Only accessible within the function
- When block 2 (try-catch) modifies `count`, it modifies `locals[0]`

**Closure variables** (in `inner`):
- Passed via `argv[0]` as a JS object containing all closure vars
- Accessed via `JS_GetPropertyUint32(ctx, argv[0], idx)`
- Shared across function boundaries

**Example from generated code:**
```c
// outer function - count is a local
static JSValue __frozen_outer(..., JSValueConst *argv) {
    JSValue locals[3];  // locals[0] = count
    // Block 0-1: Modify locals[0] directly
    // Block 2: Falls back to interpreter - LOSES local state!
}

// inner function - count is a closure var
static JSValue __frozen_inner(..., JSValueConst *argv) {
    // argv[0] = closure vars object { count: ... }
    PUSH(FROZEN_DUP(ctx, JS_GetPropertyUint32(ctx, argv[0], 0)));
}
```

**Why this matters for block-level fallback:**
- Closures already work because they're passed via argv (pointer stays valid)
- Locals DON'T work because they're C stack variables that disappear on fallback
- Need to either:
  1. Convert locals to heap storage before fallback
  2. Pass locals array pointer to JS_ExecuteBytecodeBlock

**Solution**: Pass locals via JS_ExecuteBytecodeBlock API
```c
JSValue JS_ExecuteBytecodeBlock(JSContext *ctx,
                                JSValueConst func_obj,
                                uint32_t start_pc,
                                uint32_t end_pc,
                                JSValue *stack_in_out,
                                uint32_t *sp_in_out,
                                JSValue *locals,  // ← Frozen locals array!
                                uint32_t *next_pc_out);
```

Validation on resume (debug mode only):
```c
#ifdef FROZEN_DEBUG
    // Check locals are still valid JSValues
    for (int i = 0; i < num_locals; i++) {
        if (!JS_IsValidValue(locals[i])) {
            // Corruption detected
            goto full_interpreter_fallback;
        }
    }
#endif
```

---

## Minimal Viable Implementation

**Phase 1**: Single contaminated block fallback
- Detect 1 contaminated block in otherwise clean function
- Execute contaminated block in interpreter
- Resume frozen execution after

**Phase 2**: Multiple contaminated blocks
- Track which blocks are clean/contaminated
- Generate jump table for transitions
- Handle complex control flow

**Phase 3**: Optimizations
- Inline small contaminated blocks
- Pre-compute block transitions
- Eliminate unnecessary checks

---

## QuickJS Patch Additions

```c
// In quickjs.c
/* Execute single basic block from bytecode function */
JSValue JS_ExecuteBytecodeBlock(JSContext *ctx,
                                JSValueConst func_obj,
                                uint32_t start_pc,
                                uint32_t end_pc,
                                JSValue *stack,
                                uint32_t *sp_inout,
                                JSValue *locals,
                                uint32_t *next_pc_out)
{
    // Extract bytecode
    JSObject *p = JS_VALUE_GET_OBJ(func_obj);
    JSFunctionBytecode *b = p->u.func.function_bytecode;
    
    // Set up interpreter context
    JSStackFrame sf;
    sf.stack = stack;
    sf.sp = *sp_inout;
    sf.var_buf = locals;
    sf.pc = b->byte_code_buf + start_pc;
    
    // Execute until end_pc or jump/return
    JSValue ret = js_execute_bytecode_range(ctx, &sf, b, end_pc);
    
    // Return updated state
    *sp_inout = sf.sp;
    *next_pc_out = sf.pc - b->byte_code_buf;
    
    return ret;
}
```

---

## Estimated Performance Impact

For typical partially-freezable function:
- **Before**: 1x (full interpreter)
- **After**: 10-15x (hybrid execution)

For heavily contaminated function (50% contaminated):
- **Before**: 1x (full interpreter)
- **After**: 9x (50% frozen, 50% interpreted)

**Key insight**: Even partial freezing is much better than no freezing!

---

## Next Steps

1. **Add `JS_ExecuteBytecodeBlock` to QuickJS patch**
2. **Modify `codegen_ssa.zig` to generate block PC mappings**
3. **Test with simple try-catch example**
4. **Benchmark performance improvement**
5. **Extend to complex control flow**
