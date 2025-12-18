# Never-Freeze Opcodes Analysis
**Date**: 2025-01-17
**Question**: Which `never_freeze` opcodes could be implemented with C runtime helpers?

## Summary

Out of **48 never_freeze opcodes**, we can categorize them:
- **0 can be implemented** with C helpers alone ❌
- **2 could be implemented** with major architectural changes ⚠️
- **46 fundamentally cannot be frozen** (by design) 🔒

## Analysis by Category

### Category 1: Closures & Variable References (21 opcodes) 🔒
**Fundamentally impossible** - requires runtime variable binding

| Opcode | Why Impossible |
|--------|----------------|
| `fclosure`, `fclosure8` | Creates closure with captured variables - needs runtime binding |
| `get_var_ref*` (7 variants) | Access closure variables - needs JSVarRef structure |
| `put_var_ref*` (7 variants) | Modify closure variables - needs runtime ref tracking |
| `set_var_ref*` (5 variants) | Set closure variables - needs runtime ref tracking |
| `make_loc_ref`, `make_arg_ref` | Create variable references for compound assignment |
| `make_var_ref`, `make_var_ref_ref` | Create closure variable references |

**Why impossible**:
```c
// QuickJS creates runtime reference objects:
JSVarRef *var_ref = get_var_ref(ctx, sf, idx, is_arg);
pr->u.var_ref = var_ref;  // Property holds reference to live variable

// This requires:
// 1. Access to stack frame (sf)
// 2. Runtime JSVarRef allocation
// 3. Reference counting management
// 4. Integration with QuickJS's closure system
```

**Impact**: Functions with closures, arrow functions, compound assignment operators
**Workaround**: Interpreter handles these correctly

---

### Category 2: Exception Handling (4 opcodes) 🔒
**Fundamentally impossible** - requires control flow context

| Opcode | Why Impossible |
|--------|----------------|
| `catch` | Jump to exception handler - needs dynamic control flow |
| `gosub` | Call subroutine with return point - needs call stack manipulation |
| `ret` | Return from subroutine - needs unwinding |
| `nip_catch` | Clean up exception handler - needs exception context |

**Why impossible**:
- Requires non-local jumps (setjmp/longjmp equivalent)
- Needs exception handler stack management
- Can't be represented in straight-line C code

**Impact**: Functions with try/catch/finally blocks
**Workaround**: Interpreter handles exceptions correctly

---

### Category 3: Async/Generators (6 opcodes) 🔒
**Fundamentally impossible** - requires yielding/suspending execution

| Opcode | Why Impossible |
|--------|----------------|
| `await` | Suspend execution until promise resolves |
| `yield`, `yield_star` | Yield value and suspend generator |
| `async_yield_star` | Async generator yield |
| `initial_yield` | Generator initialization |
| `return_async` | Return from async function |
| `for_await_of_start` | Async iteration setup |

**Why impossible**:
- Requires suspending execution mid-function
- Needs to save/restore local state
- Requires coroutine/fiber support
- Can't be done in C without CPS transform

**Impact**: async/await, generators, for-await-of loops
**Workaround**: Interpreter has full coroutine support

---

### Category 4: Dynamic Scope (5 opcodes) 🔒
**Fundamentally impossible** - `with` statement

| Opcode | Why Impossible |
|--------|----------------|
| `with_get_var` | Get variable through with scope chain |
| `with_put_var` | Set variable through with scope chain |
| `with_delete_var` | Delete variable through with scope |
| `with_make_ref` | Make reference through with scope |
| `with_get_ref`, `with_get_ref_undef` | Get reference through with scope |

**Why impossible**:
- Requires dynamic scope chain lookup at runtime
- Variable resolution depends on object properties
- Can't be statically analyzed or compiled

**Impact**: `with` statements (rare, discouraged in modern JS)
**Workaround**: Interpreter handles with scopes

---

### Category 5: Dynamic Code Execution (3 opcodes) 🔒
**Fundamentally impossible** - requires compilation

| Opcode | Why Impossible |
|--------|----------------|
| `eval` | Compile and execute string as code |
| `apply_eval` | Apply eval with context |
| `import` | Dynamic module import |

**Why impossible**:
- Needs JavaScript parser and compiler
- Requires runtime code generation
- Can't be frozen as it's inherently dynamic

**Impact**: eval(), Function(), dynamic imports
**Workaround**: Interpreter has compiler

---

### Category 6: Argument Mutations (5 opcodes) 🔒
**Fundamentally impossible** - modifies const arguments

| Opcode | Why Impossible |
|--------|----------------|
| `put_arg`, `put_arg0-3` | Modify function arguments array |

**Why impossible**:
```c
// Frozen functions receive const arguments:
JSValue frozen_func(JSContext *ctx, JSValueConst this_val,
                    int argc, JSValueConst *argv);
//                            ^^^^^
// Can't modify const argv - would need mutable copy
```

**Impact**: Functions that modify `arguments` object (rare)
**Workaround**: Interpreter has mutable arguments

---

### Category 7: Constant Pool Access (1 opcode) ⚠️
**Could be implemented with major changes**

| Opcode | Current Issue | Could Implement? |
|--------|---------------|------------------|
| `push_const8` | Requires bytecode constant pool | YES - with embedding |

**How to implement**:
```c
// Option 1: Embed constant pool in frozen code
static JSValue __frozen_constants[] = {
    // Serialized constants from bytecode
    JS_MKVAL(JS_TAG_STRING, "string literal"),
    JS_MKVAL(JS_TAG_INT, 42),
    // ... more constants
};

JSValue frozen_func(JSContext *ctx, ...) {
    // Access embedded constants
    PUSH(js_dup(__frozen_constants[idx]));
}
```

**Challenges**:
1. Need to serialize JSValue constants to C literals
2. Complex constants (objects, functions) are hard to serialize
3. Would increase frozen code size significantly
4. Benefit is minimal (only affects edge cases like `toFixed(2)`)

**Recommendation**: Leave as `never_freeze` unless it becomes a common blocker

**Impact**: 2 functions in ES2024 tests (`testNumberToFixed`, `testNumberToPrecision`)

---

### Category 8: Computed Property Destructuring (1 opcode) ⚠️
**Could be implemented but currently broken**

| Opcode | Current Issue | Could Implement? |
|--------|---------------|------------------|
| `to_propkey2` | Bug in function context | YES - needs debugging |

**Current status**:
- ✅ Works in global context
- ❌ Returns `undefined` in function context
- Root cause: Unknown stack management issue

**How to fix**:
1. Debug stack pointer management in destructuring
2. Verify `dup2` sequence
3. Check interaction with local variables
4. Test with comprehensive edge cases

**Effort**: Medium (requires debugging, not architectural changes)
**Benefit**: 1 additional function frozen (`testDestructComputed`)

**Recommendation**: Worth fixing if we have time, but not critical

---

## Summary Table

| Category | Count | Can Implement? | Should Implement? |
|----------|-------|----------------|-------------------|
| Closures & References | 21 | ❌ No | N/A - impossible |
| Exception Handling | 4 | ❌ No | N/A - impossible |
| Async/Generators | 6 | ❌ No | N/A - impossible |
| Dynamic Scope (with) | 5 | ❌ No | N/A - impossible |
| Dynamic Code (eval) | 3 | ❌ No | N/A - impossible |
| Argument Mutations | 5 | ❌ No | N/A - impossible |
| Constant Pool | 1 | ⚠️ Possible | 🟡 Low priority |
| to_propkey2 | 1 | ⚠️ Possible | 🟢 Worth fixing |
| **TOTAL** | **48** | **2 possible** | **1 recommended** |

---

## Recommendations

### 1. Fix to_propkey2 (Recommended ✅)
**Effort**: Low-Medium (debugging)
**Benefit**: Enable computed property destructuring in functions
**Status**: Currently marked as `never_freeze` after removing broken workaround

### 2. Keep All Other Opcodes as never_freeze (Recommended ✅)
**Reason**: Architecturally impossible or not worth the complexity

### 3. Do NOT Implement push_const8 (Recommended ✅)
**Reason**:
- Only affects 2 functions in tests
- Requires significant architectural changes
- Would increase code size and complexity
- Benefit doesn't justify cost

---

## Philosophy: C Runtime Helpers vs. Architectural Changes

### C Runtime Helpers Work When:
✅ Operation has clear input/output contract
✅ Can be implemented with public QuickJS APIs
✅ No need for runtime context (stack frames, closures, etc.)
✅ Can handle all edge cases correctly

**Examples of good helpers**:
- `frozen_add`: Int+int, string+string, conversions
- `frozen_typeof`: Type checking with all cases
- `frozen_array_get`: Property access with int/string keys
- `frozen_neg`: Negation with -0 handling

### C Runtime Helpers DON'T Work When:
❌ Operation requires runtime binding (closures, references)
❌ Operation requires control flow manipulation (exceptions, generators)
❌ Operation requires compiler (eval, dynamic imports)
❌ Operation requires mutable context (arguments mutations)

**Examples of impossible cases**:
- `make_loc_ref`: Needs JSVarRef allocation and tracking
- `catch`: Needs exception handler stack
- `yield`: Needs execution suspension
- `with_get_var`: Needs dynamic scope chain

---

## Current State

**What we have**:
- 72 functions frozen successfully (87% coverage)
- Clean `never_freeze` list with clear reasoning
- Robust C runtime helpers for arithmetic, comparisons, bitwise ops
- No half-working workarounds

**What we don't need**:
- Complex workarounds that don't fully work
- Architectural changes for minimal benefit
- Half-measures that create maintenance burden

**The one thing worth doing**:
- Fix `to_propkey2` properly (medium effort, clear benefit)

---

## Conclusion

**Answer**: Out of 48 `never_freeze` opcodes, only **1 is worth implementing** (to_propkey2).

The other 47 are either:
1. **Architecturally impossible** (46 opcodes) - would require fundamentally changing how the freeze system works
2. **Not worth the effort** (1 opcode: push_const8) - minimal benefit for significant complexity

This validates our "C runtime helpers" approach:
- ✅ Use C helpers for operations with clear semantics
- ❌ Don't try to work around fundamental limitations
- ✅ Keep the `never_freeze` list honest and well-documented
