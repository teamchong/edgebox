# QuickJS Patches

## Why We Patch QuickJS

EdgeBox's freeze system generates native C code from QuickJS bytecode for maximum performance (18x speedup for recursive functions). This requires access to internal QuickJS functions that aren't part of the public API.

## Patch 001: no-exit-on-promise-rejection (5 lines)

**Purpose:** Prevent `exit(1)` on unhandled promise rejections in WASM sandbox.

**Changes:**
- Disables stderr logging in WASI (prevents sandbox escape)
- Removes `exit(1)` call (would kill entire WASM instance)
- Allows event loop to continue gracefully

**Upstream potential:** Could propose `JS_SetPromiseRejectionHandler()` callback API to let embedders choose behavior.

## Patch 002: frozen-interpreter-helpers (16 functions, ~200 lines)

**Purpose:** Export internal QuickJS functions needed by frozen code generator.

### Iterator Protocol (4 functions) - REQUIRED

**Why needed:** Iterator state is opaque, frozen code cannot replicate the internal state machine.

- `js_frozen_for_in_start` - Initialize for-in iterator over object properties
- `js_frozen_for_in_next` - Get next property key in enumeration order
- `js_frozen_for_of_start` - Initialize for-of iterator (async capable)
- `js_frozen_for_of_next` - Get next value from iterable

**Used by:** `for-in` and `for-of` loops in frozen functions
**Example:** `for (const key in obj)` compiles to calls to these helpers

### Special Objects (4 functions) - REQUIRED

**Why needed:** These require internal QuickJS context/state not accessible via public API.

- `JS_NewArguments(ctx, argc, argv)` - Create `arguments` object (needs stack frame context)
- `JS_NewMappedArgumentsSimple(ctx, argc, argv, callee)` - Sloppy mode `arguments` with callee property
- `JS_GetImportMetaCurrent(ctx)` - Get `import.meta` (needs ES module metadata)
- `JS_GetFrozenHomeObject(ctx, func_name)` / `JS_SetFrozenHomeObject(ctx, func_name, home)` - Track `super` home objects for class methods

**Used by:** `special_object` bytecode opcode (types 0, 1, 4, 6)
**Example:** `function f() { return arguments.length; }` uses `JS_NewArguments`

### Private Fields (7 functions) - REQUIRED

**Why needed:** Private field symbol table and brand checking are internal implementation details.

- `JS_NewPrivateSymbol(ctx, atom)` - Create private symbol for `#field`
- `JS_FrozenCheckBrand(ctx, obj, func)` - Verify object has correct class brand
- `JS_FrozenAddBrand(ctx, obj, func)` - Add brand to class instance
- `js_frozen_private_in(ctx, sp)` - Check if private field exists (`#field in obj`)
- `JS_FrozenGetPrivateField(ctx, obj, name)` - Read private field value
- `JS_FrozenSetPrivateField(ctx, obj, name, val)` - Write private field value
- `JS_FrozenDefinePrivateField(ctx, obj, name, val)` - Define new private field

**Used by:** Classes with private fields (`#field` syntax)
**Example:**
```javascript
class Counter {
  #count = 0;
  increment() { this.#count++; }  // Uses private field helpers
}
```

---

## Removed Functions (Historical)

### Closure Variables (removed - never implemented)

These functions were added to the patch but never actually called by the code generator:

- `js_frozen_get_var_ref` - Stubbed out, returns undefined
- `js_frozen_set_var_ref` - Stubbed out, no-op

**Reason for removal:** Functions using closures currently fall back to interpreter mode. When closure support is added, these will need proper implementation with `JSVarRef` context passing.

---

## Maintenance Guide

### When Updating QuickJS

1. **Check for conflicts:**
   ```bash
   cd vendor/quickjs-ng
   git pull origin master
   cd ../..
   git apply --check patches/*.patch
   ```

2. **Re-apply patches:**
   ```bash
   cd vendor/quickjs-ng
   git apply ../../patches/*.patch
   ```

3. **Test freeze system:**
   ```bash
   zig build cli -Doptimize=ReleaseFast
   ./bench/run_hyperfine.sh
   ```

4. **If patches fail to apply:**
   - Check if internal function signatures changed
   - Update patch to match new QuickJS code
   - Test that frozen functions still work correctly

### Patch Statistics

- **Total functions patched:** 20 (16 from patch 002 + 2 from patch 003 + 1 from patch 004 + 1 from patch 005)
- **Lines of patch code:** ~280
- **QuickJS files modified:** 2 (quickjs.c, quickjs.h)
- **Maintenance burden:** Low (functions rarely change)
- **Opcodes now freezable:** +3 (`push_const8`, `fclosure`, `fclosure8`)

---

## Upstream Progress

**Status:** Monitoring [quickjs-ng](https://github.com/quickjs-ng/quickjs) for AOT compilation interest.

If QuickJS-NG adds official AOT APIs, we can:
1. Migrate to the official API
2. Remove our patches entirely
3. Reduce maintenance burden to zero

**Potential upstream proposal:**
```c
/* Official AOT Helper API */
typedef struct {
    int (*for_in_start)(JSContext *, JSValue *);
    int (*for_in_next)(JSContext *, JSValue *);
    int (*for_of_start)(JSContext *, JSValue *, int);
    int (*for_of_next)(JSContext *, JSValue *, int);
} JSIteratorHelpers;

JS_EXTERN const JSIteratorHelpers* JS_GetIteratorHelpers(void);
```

## Patch 003: frozen-interpreter-dynamic-import (2 functions)

**Purpose:** Enable `import()` support in frozen functions by exposing dynamic import internals.

**Changes:**
- Export `js_dynamic_import(ctx, specifier)` - Main dynamic import function
- Export `js_dynamic_import_job(ctx, argc, argv)` - Async import job handler

**Why needed:** Frozen C functions don't have bytecode stack frames, so `JS_GetScriptOrModuleName()` returns NULL. The frozen interpreter needs to:
1. Call `js_dynamic_import_job` directly with explicit basename parameter
2. Pass module context (basename) that would normally be extracted from stack frame

**Implementation:** `frozen_dynamic_import()` in `frozen_runtime.c` wraps these functions, providing explicit basename from static frozen function variables.

**Used by:** `import()` expression in frozen functions
**Example:**
```javascript
function loadModule(path) {
  return import(path);  // Now works in frozen code
}
```

**Without patch:** `import()` would fail with "no function filename for import()" because frozen C functions lack stack frame context.

## Patch 004: frozen-interpreter-constant-pool (1 function)

**Purpose:** Enable `push_const8` opcode support in frozen functions by providing access to the bytecode constant pool.

**Changes:**
- Export `JS_GetFunctionConstantPool(ctx, func_obj, *pcount)` - Extract constant pool from bytecode function

**Why needed:** The `push_const8` opcode pushes constants (strings, numbers, objects) from the bytecode constant pool. Frozen C functions don't have direct access to the pool, so we:
1. Extract `cpool` pointer from original bytecode function during initialization
2. Store it in static variables (`_<funcname>_cpool` and `_<funcname>_cpool_count`)
3. Access pool via these statics when executing `push_const8`

**Used by:** `push_const8` opcode in frozen functions
**Example:**
```javascript
function formatNumber(num) {
  return num.toFixed(2);  // "2" is pushed via push_const8
}
```

**Blocks removed:** Functions using string/number literals beyond simple integers can now be frozen (previously blocked by `push_const8`).

## Patch 005: frozen-interpreter-fclosure (1 function)

**Purpose:** Enable nested function creation in frozen functions by exposing closure creation internals.

**Changes:**
- Export `js_closure(ctx, bfunc, cur_var_refs, sf)` - Create function closure with captured variables

**Why needed:** The `fclosure` opcode creates nested functions that capture outer function variables. Frozen C functions need to:
1. Get the nested function template from constant pool (index from opcode operand)
2. Call `js_closure()` to wrap it with the current function's `var_refs` array
3. Push the resulting closure onto the stack

**Implementation:** Frozen functions receive `var_refs` via `patch_closure_hooks.js` (or NULL if no closures). The `fclosure` handler:
- Duplicates the function template from `cpool[idx]`
- Calls `js_closure(ctx, bfunc, __var_refs, NULL)` to create the closure
- Handles both `fclosure` (u32 index) and `fclosure8` (u8 index) variants

**Used by:** `fclosure` and `fclosure8` opcodes in frozen functions
**Example:**
```javascript
function createCounter(start) {
  let count = start;
  return function increment() {  // This inner function uses fclosure
    count++;
    return count;
  };
}
```

**Without patch:** Nested functions would return undefined, breaking closures entirely.

---

## Known Issues

### Home Object Registry Implementation

**Current status:** Uses global array with `strcmp` for function name lookup.

**Limitations:**
- Not thread-safe (global state)
- Fixed 256 entry limit
- No cleanup (memory leak on context destruction)

**Better implementation (TODO):**
- Use per-JSContext storage via `JS_SetContextOpaque`
- Or store home object as hidden property on function object
- Or use QuickJS's existing property storage mechanisms

**Impact:** Low - most code doesn't use `super` keyword, registry works for typical use cases.

---

## Summary

These patches enable EdgeBox to generate highly optimized native C code from JavaScript, achieving **18x speedup** for recursive functions compared to interpreted execution. The patches are minimal, well-tested, and maintained with each QuickJS update.

**Current patch efficiency:**
- 20 functions exported (all actively used)
- 0 dead code
- Well-documented rationale for each function
- 3 new opcodes now freezable (import, push_const8, fclosure/fclosure8)
