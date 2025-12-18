# Opcodes That Prevent Function Freezing

This document lists all opcodes that prevent functions from being frozen (compiled to optimized C code).

**Last Updated**: 2025-01-18 after fclosure8/set_name fix

## Categories of Unsupported Opcodes

### 1. Exception Handling (3 opcodes)
- **`catch`** - Try-catch exception handling
- **`nip_catch`** - Exception cleanup/finalization
- **`ret`** - Return from try-finally or generator

**Impact**: Functions with `try-catch`, `try-finally`, or complex control flow cannot be frozen.

**Example**:
```javascript
function test() {
    try {
        doSomething();
    } catch (e) {  // Uses 'catch' opcode
        handleError(e);
    }
}
```

---

### 2. Async/Await (3 opcodes)
- **`await`** - Await promise resolution
- **`return_async`** - Return from async function
- **`for_await_of_start`** - Async iteration start

**Impact**: All async functions cannot be frozen.

**Example**:
```javascript
async function fetchData() {
    const result = await fetch(url);  // Uses 'await' opcode
    return result;
}
```

---

### 3. Generators (4 opcodes)
- **`yield`** - Yield value from generator
- **`yield_star`** - Yield* delegation
- **`async_yield_star`** - Async yield* delegation
- **`initial_yield`** - Generator initialization

**Impact**: All generator functions cannot be frozen.

**Example**:
```javascript
function* generator() {
    yield 1;  // Uses 'yield' opcode
    yield 2;
}
```

---

### 4. Mutable Arguments (5 opcodes)
- **`put_arg`** - Modify arguments object
- **`put_arg0`** - Modify arguments[0]
- **`put_arg1`** - Modify arguments[1]
- **`put_arg2`** - Modify arguments[2]
- **`put_arg3`** - Modify arguments[3]

**Impact**: Functions that modify the `arguments` object cannot be frozen.

**Reason**: Frozen functions receive arguments as `const JSValueConst *argv`, which cannot be modified.

**Example**:
```javascript
function modifyArgs() {
    arguments[0] = 42;  // Uses 'put_arg0' opcode - NOT FREEZABLE
}
```

---

### 5. Dynamic Scope (with statement) (5 opcodes)
- **`with_get_var`** - Get variable from with scope
- **`with_put_var`** - Set variable in with scope
- **`with_delete_var`** - Delete variable from with scope
- **`with_get_ref`** - Get reference from with scope
- **`with_get_ref_undef`** - Get undefined reference from with scope
- **`with_make_ref`** - Make reference in with scope

**Impact**: Functions using `with` statement cannot be frozen.

**Note**: `with` is deprecated in strict mode anyway.

**Example**:
```javascript
function useWith(obj) {
    with (obj) {  // Uses with_* opcodes
        console.log(prop);
    }
}
```

---

### 6. Reference Creation (3 opcodes)
- **`make_arg_ref`** - Create reference to arguments
- **`make_loc_ref`** - Create reference to local variable
- **`make_var_ref`** - Create reference to closure variable
- **`make_var_ref_ref`** - Create reference to reference

**Impact**: Functions that create references to local variables (for modification by nested functions) cannot be frozen.

**Example**:
```javascript
function outer() {
    let x = 0;
    function inner() {
        x++;  // May use make_loc_ref if x is captured
    }
    return inner;
}
```

---

### 7. Control Flow (1 opcode)
- **`gosub`** - Subroutine call (used in finally blocks)

**Impact**: Functions with `finally` blocks may use this opcode.

---

### 8. Dynamic Code Execution (2 opcodes)
- **`eval`** - Direct eval() call
- **`apply_eval`** - Apply with eval context

**Impact**: Functions using `eval()` cannot be frozen.

**Example**:
```javascript
function useEval() {
    eval("console.log('hello')");  // Uses 'eval' opcode
}
```

---

### 9. Invalid/Unknown (1 opcode)
- **`invalid`** - Invalid opcode (should never appear)

---

## Summary Statistics

**Total Never-Freeze Opcodes**: 30

**By Category**:
- Exception handling: 3
- Async/Await: 3
- Generators: 4
- Mutable arguments: 5
- Dynamic scope (with): 6
- Reference creation: 4
- Control flow: 1
- Dynamic execution: 2
- Invalid: 1

## What CAN Be Frozen

Functions that use ONLY these features can be frozen:
- ✅ Pure computation (arithmetic, comparisons, logic)
- ✅ Local variables and parameters
- ✅ Function calls (including recursive)
- ✅ Arrays and objects (creation and access)
- ✅ Loops (for, while, do-while, for-in, for-of)
- ✅ Switch statements
- ✅ Conditional expressions (if/else, ternary)
- ✅ Named function expressions (since fclosure8/set_name fix)
- ✅ String concatenation
- ✅ Template literals
- ✅ Spread operator
- ✅ Destructuring
- ✅ Regular expressions
- ✅ typeof, instanceof, in operators
- ✅ Property access (dot notation, bracket notation)

## Performance Impact

Functions that CAN be frozen achieve:
- **18x speedup** for compute-intensive code (e.g., `fib(35)`)
- **Direct C recursion** instead of JS function calls
- **No JSValue boxing** for int32 arithmetic in hot path
- **Compile-time stack analysis** for optimal register usage

Functions that CANNOT be frozen:
- Fall back to QuickJS interpreter (normal performance)
- No error or warning - graceful degradation
- Still benefit from QuickJS-NG optimizations (fast interpreter, JIT if enabled)

## Future Work

To expand freezing coverage, we could implement:

1. **Basic try-catch support** - Handle simple exception patterns
2. **Simple async/await** - For Promise-based code without complex state machines
3. **Immutable arguments** - Freeze functions that read but don't modify arguments
4. **Reference tracking** - Analyze closure variable usage to freeze more closures

However, these are complex and would require significant QuickJS integration.
