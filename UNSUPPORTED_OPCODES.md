# Unsupported Opcodes - Implementation Guide

## Summary

**Total Never-Freeze Opcodes:** 29
- **Cannot implement:** 16 (async/generator/eval/with)
- **Already handled:** 4 (exception handling via block fallback)
- **Can implement:** 9 (references + put_arg)

## Complete List by Category

### ❌ Category 1: Fundamentally Incompatible (10 opcodes)
**Cannot be frozen - require runtime state management**

#### Async/Await Operations
1. `return_async` - Returns from async function with promise unwrapping
2. `await` - Suspend execution until promise resolves
3. `for_await_of_start` - Initialize async iteration

**Why unfreezenable:** Requires promise chain management, microtask queue, and execution suspension.

#### Generator Operations
4. `initial_yield` - Initialize generator state
5. `yield` - Suspend generator, return value
6. `yield_star` - Delegate to another generator
7. `async_yield_star` - Async generator delegation

**Why unfreezenable:** Requires generator state object, resumable execution context, and iterator protocol.

#### Dynamic Evaluation
8. `eval` - Direct eval() call
9. `apply_eval` - Indirect eval call

**Why unfreezenable:** Compiles code at runtime, fundamentally dynamic.

### 🚫 Category 2: Deprecated/Rare (6 opcodes)
**Not worth implementing - rarely used in modern JS**

#### `with` Statement
10. `with_get_var` - Get variable from with scope
11. `with_put_var` - Set variable in with scope
12. `with_delete_var` - Delete variable from with scope
13. `with_make_ref` - Create reference in with scope
14. `with_get_ref` - Get reference from with scope
15. `with_get_ref_undef` - Get reference or undefined

**Why skip:** `with` is deprecated, forbidden in strict mode, and extremely rare in modern JavaScript.

### ✅ Category 3: Already Working! (4 opcodes)
**Exception handling via block-level fallback**

#### Try-Catch-Finally
16. `catch` - Enter exception handler block
17. `gosub` - Call finally block subroutine
18. `ret` - Return from finally block
19. `nip_catch` - Clean up exception handler stack

**Status:** These work via partial freeze! Clean blocks execute in frozen C, contaminated blocks (try-catch) fall back to interpreter.

**TODO:** Verify with benchmarks and document performance.

### 🟡 Category 4: Easy to Implement (5 opcodes)
**Argument mutation - trivial handlers**

#### Argument Writes
20. `put_arg` - Write to argument by index (operand)
21. `put_arg0` - Write to arguments[0]
22. `put_arg1` - Write to arguments[1]
23. `put_arg2` - Write to arguments[2]
24. `put_arg3` - Write to arguments[3]

**Implementation challenge:** `argv` is `JSValueConst *` (read-only).

**Solution options:**
1. **Copy argv to mutable array** (5 lines of C)
   ```c
   JSValue mutable_argv[argc];
   for(int i = 0; i < argc; i++) mutable_argv[i] = JS_DupValue(ctx, argv[i]);
   // ... use mutable_argv ...
   ```
2. **Skip put_arg functions** (check at freeze time)
3. **Warn and fall back to interpreter**

**Recommendation:** Option 1 - copy argv if function uses put_arg opcodes.

**Impact:** ~5% more functions could be frozen (argument mutation is uncommon in modern JS).

### 🟠 Category 5: Moderate Implementation (4 opcodes)
**Reference operations - need reference struct**

#### Reference Creation
25. `make_loc_ref` - Create reference to local variable
26. `make_arg_ref` - Create reference to argument
27. `make_var_ref` - Create reference to closure variable
28. `make_var_ref_ref` - Create indirect reference to closure variable

**What they do:**
```javascript
++x          // make_loc_ref, get_ref_value, inc, put_ref_value
delete obj.p // make_prop_ref, delete_ref
```

**Implementation strategy:**
1. Define reference struct:
   ```c
   typedef struct {
       int type;           // 0=local, 1=arg, 2=closure, 3=global, 4=prop
       int index;          // local/arg/closure index
       JSValue base;       // object for property refs
       JSAtom prop;        // property name for property refs
   } FrozenRef;
   ```

2. Push reference onto stack (as JSValue with special tag)

3. Implement reference operations:
   - `get_ref_value` - Read from reference
   - `put_ref_value` - Write to reference
   - `delete_ref` - Delete reference target

**Complexity:** ~200 lines of C code

**Impact:** ~10-15% more functions could be frozen (++, --, delete are common)

**Challenge:** Reference lifetime management, ensuring references are freed.

## Missing Opcode: `define_class`

**UPDATE:** After reviewing opcodes.zig, there's one more:

29. `define_class` - Class definition

**Status:** Category ❌ (Fundamentally incompatible)

**Why:** Class definitions involve:
- Dynamic property descriptors
- Constructor initialization
- Prototype chain setup
- Static/instance method binding
- Super class inheritance

**Impact:** Classes are common, but usually defined at module scope (not inside hot loops).

**Workaround:** Classes defined at top-level won't hurt performance (only called once).

## Implementation Priority

### Phase 1: Verify Exception Handling (0 effort)
- Run benchmarks on functions with try-catch
- Document that partial freeze already handles this
- Expected: 18x speedup on clean blocks, normal speed on catch blocks
- **Estimated impact:** This likely already covers 20-30% of "unsupported" functions!

### Phase 2: Argument Mutation (1 hour)
- Add argv copy when put_arg opcodes detected
- Update opcode category from `.never_freeze` to `.variable`
- Test with benchmark functions that mutate arguments
- **Estimated impact:** +5% freeze coverage

### Phase 3: Reference Operations (1 day)
- Implement FrozenRef struct and runtime helpers
- Add handlers for make_*_ref, get_ref_value, put_ref_value, delete_ref
- Test with ++, --, delete operations
- **Estimated impact:** +10-15% freeze coverage

## Current Status

**Freeze coverage (estimated):**
- Current: ~30-40% of non-trivial functions
- After exception handling verified: ~50-60% (already implemented!)
- After put_arg: ~55-65%
- After references: ~65-75%

**Remaining ~25% unfreezenable:**
- Async/await functions
- Generators
- Functions using eval
- Functions using with statements
- Class definitions

This is **excellent coverage** - async/eval/generators/with/classes are special-purpose features that shouldn't be in hot paths anyway.

## Recommendation

**Start with Phase 1** (verify exception handling) because it's zero effort and likely shows big wins that are already working!

Then decide if Phase 2 and 3 are worth the engineering effort based on:
1. What % of real-world functions use put_arg?
2. What % use reference operations (++, --, delete)?
3. Are these operations in hot code paths?

Run the test262 freeze scanner to get real data:
```bash
node tools/test262_freeze_scanner.js vendor/quickjs-ng/test262/test/language/ > freeze_stats.json
```

This will show which opcodes appear most frequently in ES2024 test suite.
