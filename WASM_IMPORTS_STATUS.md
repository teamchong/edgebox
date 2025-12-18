# WASM Imports Implementation Status

## Summary

Implementing zero-overhead WASM module imports: `import * as math from "./math.wasm"`

**Progress:** Phase 1 & 2 Complete (60% done) | Estimated remaining: ~4-5 hours

## Completed ✅

### Phase 1: Foundation (Complete - Committed 6be5a3e)
- ✅ `src/wasm_import_loader.zig`: WASM module registry with multi-instance support
  - `WasmModuleRegistry`: Tracks loaded modules by path
  - `loadModule()`: Loads WASM files on-demand
  - `extractExports()`: Discovers function exports from module metadata
  - `callFunction()`: Executes WASM functions with type conversion
- ✅ `src/wasm_js_bridge.zig`: Bidirectional type conversion
  - `jsValueToWasmVal()`: JS → WASM value conversion
  - `wasmValToJSValue()`: WASM → JS value conversion
  - Supports: i32, i64, f32, f64, bool, null/undefined
- ✅ `WASM_IMPORTS_DESIGN.md`: Complete implementation plan

### Phase 2: Host Functions (Complete - Committed 0910c3b)
- ✅ `__edgebox_wasm_import(path_offset, path_len)`: Load WASM module by path
  - Loads module into registry
  - Returns success/failure indicator
- ✅ `__edgebox_wasm_call(path, func_name, args, args_count)`: Call WASM function
  - Looks up loaded module by path
  - Converts i32 args to wasm_val_t
  - Calls function via WAMR API
  - Returns i32 result
- ✅ Symbol registration in WAMR native functions table
- ✅ Registry initialization in main() with lifecycle management

### Phase 3.2: Test WASM Module (Complete)
- ✅ `test_wasm/math.zig`: Simple math WASM module
  - `add(a, b)`: Addition
  - `multiply(a, b)`: Multiplication
  - `subtract(a, b)`: Subtraction
  - `fibonacci(n)`: Iterative Fibonacci
- ✅ `test_wasm/build.zig`: Build script for WASM compilation
- ✅ Compiled `test_wasm/zig-out/bin/math.wasm` (5.2KB)

## In Progress 🚧

### Phase 3.3: JS Test File (Placeholder Created)
- ⚠️ `bench/test_wasm_import.js`: Test skeleton created
- ⏸️ Waiting for JS polyfill wrapper before it can run

## Remaining Work ⏭️

### Phase 3.1: JS Polyfill Wrapper (~2-3 hours)
**File:** `src/polyfills/runtime.js`

Need to add JavaScript wrapper that:
1. Declares native host functions
2. Implements `__edgebox_wasm_load(path)` wrapper
3. Creates module namespace object with wrapped exports
4. For each export, creates JS function that calls `__edgebox_wasm_call()`

**Example:**
```javascript
// Declare native functions (imported from WASM host)
const native_wasm_import = 1;  // Symbol: edgebox_wasm.wasm_import
const native_wasm_call = 2;    // Symbol: edgebox_wasm.wasm_call

// Wrapper function to load WASM module
globalThis.__edgebox_wasm_load = function(path) {
    // Call native host function to load module
    const pathBuf = stringToWasmMemory(path);
    const result = wasmCallNative(native_wasm_import, pathBuf.offset, pathBuf.len);

    if (result === 0) {
        throw new Error(`Failed to load WASM module: ${path}`);
    }

    // Create module namespace with callable exports
    // For now, hardcode exports (TODO: query exports from registry)
    const exports = {
        add: (a, b) => callWasmFunction(path, "add", [a, b]),
        multiply: (a, b) => callWasmFunction(path, "multiply", [a, b]),
        subtract: (a, b) => callWasmFunction(path, "subtract", [a, b]),
        fibonacci: (n) => callWasmFunction(path, "fibonacci", [n]),
    };

    return exports;
};

// Helper: Call WASM function by name
function callWasmFunction(path, funcName, args) {
    const pathBuf = stringToWasmMemory(path);
    const funcBuf = stringToWasmMemory(funcName);
    const argsBuf = int32ArrayToWasmMemory(args);

    const result = wasmCallNative(
        native_wasm_call,
        pathBuf.offset, pathBuf.len,
        funcBuf.offset, funcBuf.len,
        argsBuf.offset, args.length
    );

    return result;
}
```

**Challenges:**
- Need to understand how EdgeBox's runtime.js accesses native functions
- May need to add string→memory and array→memory helpers
- Need to handle WASM memory allocation for string/array args

### Phase 3.4: QuickJS Import Hook (~1-2 hours)
**Option A: Patch QuickJS (More invasive, but standard)**
- Modify `js_dynamic_import()` in quickjs.c to detect `.wasm` extension
- Call `__edgebox_wasm_load()` for WASM files
- Return module namespace object

**Option B: User-space polyfill (Simpler, less invasive)**
- Add helper: `globalThis.__wasm_import = (path) => __edgebox_wasm_load(path)`
- User code: `const math = await __wasm_import("./math.wasm")`
- Skip dynamic import syntax entirely

**Recommendation:** Start with Option B for MVP, consider Option A later.

### Phase 3.5: Testing & Debugging (~1-2 hours)
1. Run `bench/test_wasm_import.js` with EdgeBox
2. Verify:
   - Module loads successfully
   - Functions are callable
   - Correct results returned
   - Error handling works
3. Add benchmarks comparing performance

## Architecture Decisions

### Type System Compatibility Issue (Solved ✅)
**Problem:** Zig's `@cImport` creates separate type namespaces per file.
- `edgebox_wamr.zig` has `c.wasm_val_t`
- `wasm_import_loader.zig` has its own `c.wasm_val_t`
- These are incompatible types despite being structurally identical

**Solution:**
1. Export `wasm_import_loader.c` as `pub const c`
2. Use `@ptrCast()` to convert between namespaces
3. Use `@constCast()` for WAMR API that expects mutable pointers

### Module Registry Design
**Key insight:** Use file path as module ID (not integer handle)
- Simpler: No need to manage ID allocation
- Natural: Path is what user provides in `import` statement
- Efficient: HashMap lookup by path is O(1)

### Host Function Signature
**Current:** `(path, func_name, args_array, args_count) → i32`
- Simple: Only supports i32 args and results
- Future: Could extend to support f32/f64/i64 via tagged unions

## Performance Expectations

### Current Implementation
**JS → Host → WASM Overhead:** ~100-200ns per call
- String memory copies (path, function name)
- Args array allocation
- WAMR function lookup
- Execution environment creation

### Theoretical Optimal (Future)
**Direct WASM-to-WASM Linkage:** ~10-20ns per call
- Would require WAMR JIT to inline across module boundaries
- Significant complexity, may not be worth it

### Comparison
| Approach | Overhead | Complexity |
|----------|----------|------------|
| Current (host functions) | ~100-200ns | Low |
| Direct linking | ~10-20ns | Very high |
| Native JS function | ~1-2ns | N/A (baseline) |

**Conclusion:** 100-200ns overhead is acceptable for most use cases.

## Next Steps

1. **Implement JS polyfill wrapper** in `src/polyfills/runtime.js`
   - Study existing pattern for calling native functions
   - Add `__edgebox_wasm_load()` wrapper
   - Add helper functions for memory allocation
2. **Test with simple example**
   - Run `bench/test_wasm_import.js`
   - Fix any issues
3. **Add QuickJS import hook (optional)**
   - Start with user-space helper (`__wasm_import`)
   - Consider dynamic import patch later
4. **Documentation**
   - Update README with WASM import usage
   - Add performance notes
   - Document limitations (i32 only for now)

## Known Limitations

1. **Type Support:** Only i32 args/results currently supported
   - f32/f64/i64 would need additional wrapper logic
   - Could add in future if needed

2. **Export Discovery:** Currently hardcoded exports in JS wrapper
   - Could add `__edgebox_wasm_get_exports()` host function
   - Would return JSON array of export names

3. **Memory Management:** No automatic cleanup of loaded modules
   - Modules stay in registry until program exit
   - Could add explicit unload function if needed

4. **Error Handling:** Limited error reporting
   - Host functions return 0 on error with debug prints
   - Could improve by returning error codes or exception objects

## Questions / Decisions Needed

- **Q:** Should we support dynamic import syntax (`import * as math from "./math.wasm"`) or user-space helper (`__wasm_import("./math.wasm")`)?
  - **A:** Start with user-space helper for MVP simplicity

- **Q:** Should we query exports dynamically or hardcode them?
  - **A:** Hardcode for MVP, add query function later if needed

- **Q:** Should we extend type support beyond i32?
  - **A:** Not for MVP, add if users request it
