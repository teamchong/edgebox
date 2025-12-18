# WASM Import Support - Implementation Design

## Goal

Enable JS code to import and call WASM modules with zero-overhead:

```javascript
import * as math from "./math.wasm";
console.log(math.add(5, 3));        // Direct WASM-to-WASM call
console.log(math.multiply(4, 7));   // No JS→Zig→WASM overhead
```

## Architecture

```
┌─────────────────────────────────────┐
│  User JS Code                       │
│  import * as wasm from "./lib.wasm" │
└──────────────┬──────────────────────┘
               │
               ▼
┌─────────────────────────────────────┐
│  QuickJS (WASM #1 - main sandbox)   │
│  - Registers WASM imports           │
│  - Calls via direct WASM linkage    │
└──────────────┬──────────────────────┘
               │ (zero-copy calls)
               ▼
┌─────────────────────────────────────┐
│  User WASM Module (WASM #2)         │
│  - Loaded as separate WASM instance │
│  - Shares WAMR runtime              │
│  - Direct function calls (no FFI)   │
└─────────────────────────────────────┘
```

## Implementation Status

### ✅ Completed (Phase 1)

**1. WAMR Multi-Instance Support** (`src/wasm_import_loader.zig`)
- `WasmModuleRegistry`: Tracks loaded WASM modules
- `loadModule()`: Loads WASM files on-demand
- `extractExports()`: Discovers function exports
- `callFunction()`: Executes WASM functions with args/results

**2. JS↔WASM Value Conversion** (`src/wasm_js_bridge.zig`)
- `jsValueToWasmVal()`: Converts JSValue → wasm_val_t
- `wasmValToJSValue()`: Converts wasm_val_t → JSValue
- Supports: i32, i64, f32, f64, bool
- Handles type coercion (JS number → WASM int/float)

### 🔄 In Progress (Phase 2)

**3. Host Function Registration**
- Need to add `__edgebox_wasm_import(path)` host function
- Returns JS object with wrapped WASM exports
- Pattern: Same as existing `__edgebox_http_dispatch`, etc.

**4. QuickJS Import Hook**
- Need to patch QuickJS `js_dynamic_import()` to detect `.wasm` extension
- Call `__edgebox_wasm_import()` for WASM files
- Return module namespace object with exports

### ❌ TODO (Phase 3)

**5. WASM Function Wrapper**
- Create generic JS function wrapper for each WASM export
- Wrapper: Convert JS args → WASM vals → call → convert result → JSValue
- Use `JS_NewCFunction2` with magic value for function pointer

**6. Testing Infrastructure**
- Create example WASM module (Rust/C/Zig → WASM)
- Test: import, call functions, type conversion
- Benchmark: Measure overhead vs native WASM calls

**7. Documentation**
- User guide: How to build and import WASM modules
- Examples: math.wasm, crypto.wasm, etc.
- Performance notes: When to use WASM vs JS

## Detailed Implementation Plan

### Phase 2.1: Host Function (1-2 hours)

**File:** `src/edgebox_wamr.zig`

Add host function:
```zig
// Host function: Load WASM module and return JS object with exports
fn __edgebox_wasm_import(
    exec_env: c.wasm_exec_env_t,
    path_offset: i32,
    path_len: i32,
) callconv(.C) i64 {
    const ctx = getQuickJSContext(exec_env) orelse return js_exception();
    const memory = getWasmMemory(exec_env) orelse return js_exception();

    // Get path string from WASM memory
    const path_slice = memory[@intCast(path_offset)..@intCast(path_offset + path_len)];

    // Load WASM module
    const registry = wasm_import_loader.getGlobalRegistry() orelse return js_exception();
    const module = registry.loadModule(path_slice) catch |err| {
        return jsThrowError(ctx, "Failed to load WASM: {}", .{err});
    };

    // Create JS object for exports
    const exports_obj = qjs.JS_NewObject(ctx);

    // Wrap each export as JS function
    var iter = module.exports.iterator();
    while (iter.next()) |entry| {
        const export_func = entry.value_ptr.*;

        // Create JS function wrapper
        const js_func = qjs.JS_NewCFunction2(
            ctx,
            wasmCallWrapper,
            export_func.name.ptr,
            @intCast(export_func.signature.param_count),
            .c_function,
            @intFromPtr(export_func.func_ptr), // magic = function pointer
        );

        qjs.JS_SetPropertyStr(ctx, exports_obj, export_func.name.ptr, js_func);
    }

    return @bitCast(exports_obj);
}

// Global symbol for registration
var g_wasm_import_symbols = [_]NativeSymbol{
    .{ .symbol = "wasm_import", .func_ptr = @ptrCast(@constCast(&__edgebox_wasm_import)), .signature = "(ii)i", .attachment = null },
};

// Register in registerHostFunctions()
fn registerHostFunctions() void {
    // ... existing registrations ...
    _ = c.wasm_runtime_register_natives("edgebox_wasm", &g_wasm_import_symbols, g_wasm_import_symbols.len);
}
```

### Phase 2.2: WASM Call Wrapper (2-3 hours)

**File:** `src/edgebox_wamr.zig`

Generic wrapper for calling WASM functions:
```zig
fn wasmCallWrapper(
    ctx: *qjs.JSContext,
    this_val: qjs.JSValue,
    argc: c_int,
    argv: [*c]qjs.JSValue,
    magic: c_int, // WASM function pointer
) callconv(.C) qjs.JSValue {
    _ = this_val;

    // Get WASM function pointer from magic
    const func_ptr: c.wasm_function_inst_t = @ptrFromInt(@as(usize, @intCast(magic)));

    // Get module and registry
    const registry = wasm_import_loader.getGlobalRegistry() orelse {
        return qjs.JS_ThrowInternalError(ctx, "WASM registry not initialized");
    };

    // Find module that owns this function (TODO: optimize with reverse lookup)
    var module: ?*wasm_import_loader.WasmModuleRegistry.LoadedModule = null;
    var iter = registry.modules.iterator();
    while (iter.next()) |entry| {
        var exports_iter = entry.value_ptr.*.exports.iterator();
        while (exports_iter.next()) |export_entry| {
            if (export_entry.value_ptr.func_ptr == func_ptr) {
                module = entry.value_ptr.*;
                break;
            }
        }
        if (module != null) break;
    }

    if (module == null) {
        return qjs.JS_ThrowInternalError(ctx, "WASM function not found in registry");
    }

    // Convert JS args to WASM vals
    var wasm_args: [16]wamr.wasm_val_t = undefined;
    const arg_count = @min(@as(usize, @intCast(argc)), 16);
    for (0..arg_count) |i| {
        wasm_args[i] = wasm_js_bridge.jsValueToWasmVal(ctx, argv[i]);
    }

    // Prepare result buffer
    var wasm_results: [16]wamr.wasm_val_t = undefined;

    // Call WASM function
    registry.callFunction(module.?, "function_name", wasm_args[0..arg_count], wasm_results[0..1]) catch |err| {
        return qjs.JS_ThrowInternalError(ctx, "WASM call failed: {}", .{err});
    };

    // Convert result to JS
    return wasm_js_bridge.wasmValToJSValue(ctx, wasm_results[0]);
}
```

### Phase 3.1: QuickJS Import Hook (3-4 hours)

**File:** `patches/quickjs-import-wasm.patch`

Patch QuickJS to detect `.wasm` imports:
```c
// In quickjs.c, find js_dynamic_import() function

static JSValue js_dynamic_import(JSContext *ctx, JSValueConst specifier) {
    const char *str;
    size_t len;

    str = JS_ToCStringLen(ctx, &len, specifier);
    if (!str)
        return JS_EXCEPTION;

    // Check for .wasm extension
    if (len > 5 && strcmp(str + len - 5, ".wasm") == 0) {
        // Call host function to load WASM module
        JSValue global = JS_GetGlobalObject(ctx);
        JSValue wasm_import_fn = JS_GetPropertyStr(ctx, global, "__edgebox_wasm_import");
        JS_FreeValue(ctx, global);

        if (JS_IsFunction(ctx, wasm_import_fn)) {
            JSValue args[] = { specifier };
            JSValue result = JS_Call(ctx, wasm_import_fn, JS_UNDEFINED, 1, args);
            JS_FreeValue(ctx, wasm_import_fn);
            return result;
        }
        JS_FreeValue(ctx, wasm_import_fn);
    }

    // Fallback to normal JS import
    return js_dynamic_import_job(ctx, specifier);
}
```

**Integration:**
1. Add patch to `tools/patches/quickjs-import-wasm.patch`
2. Apply in `tools/inject_hooks.js` or build.zig
3. Rebuild QuickJS with patch

### Phase 3.2: Testing (2-3 hours)

**Create test WASM module:**

```rust
// test_wasm/src/lib.rs
#[no_mangle]
pub extern "C" fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[no_mangle]
pub extern "C" fn multiply(a: i32, b: i32) -> i32 {
    a * b
}

#[no_mangle]
pub extern "C" fn fibonacci(n: i32) -> i32 {
    if n <= 1 { return n; }
    fibonacci(n - 1) + fibonacci(n - 2)
}
```

**Build:**
```bash
cd test_wasm
cargo build --target wasm32-wasi --release
cp target/wasm32-wasi/release/test_wasm.wasm ../bench/math.wasm
```

**Test:**
```javascript
// bench/test_wasm_import.js
import * as math from "./math.wasm";

console.log("Testing WASM imports:");
console.log("add(5, 3) =", math.add(5, 3));              // Expected: 8
console.log("multiply(4, 7) =", math.multiply(4, 7));    // Expected: 28
console.log("fibonacci(10) =", math.fibonacci(10));      // Expected: 55

// Benchmark
const start = Date.now();
for (let i = 0; i < 100000; i++) {
    math.add(i, i + 1);
}
const elapsed = Date.now() - start;
console.log(`Benchmark: ${elapsed}ms for 100k calls`);
```

**Run:**
```bash
zig build cli -Dsource-dir=bench -Doptimize=ReleaseFast
./zig-out/bin/edgebox zig-out/bin/bench/test_wasm_import.wasm
```

## Performance Expectations

### Direct WASM Call (Current - Stub)
```
JS → (stub error) → Exception
Overhead: N/A (not working)
```

### With This Implementation
```
JS → QuickJS → WASM function wrapper → WAMR call → User WASM
Overhead: ~100-200ns per call (type conversion + function dispatch)
```

### Theoretical Optimal (Future: Direct Linking)
```
JS → Inline WASM call → User WASM
Overhead: ~10-20ns per call (nearly zero)
```

**Note:** To achieve theoretical optimal, we'd need:
1. WAMR JIT compilation (compile both modules together)
2. Function inlining across module boundaries
3. This is complex and may not be worth the effort

## Alternatives Considered

### 1. Emscripten-style JS Glue Code
**Pros:** Standard, well-tested
**Cons:** Large bundle size, slow startup, not zero-overhead

### 2. WASM Component Model
**Pros:** Future standard, composable
**Cons:** Not widely supported yet, complex tooling

### 3. Our Approach: Direct WAMR Multi-Instance
**Pros:** Zero-overhead, simple, works today
**Cons:** EdgeBox-specific, not portable

## Effort Estimate

- **Phase 1 (Multi-instance + conversion):** ✅ Complete (~4 hours)
- **Phase 2 (Host function + wrapper):** ~4-5 hours
- **Phase 3 (QuickJS patch + testing):** ~5-6 hours
- **Total:** ~13-15 hours of focused work

## Recommendation

**Yes, this is doable and worth implementing!**

**Why:**
1. Enables real WASM execution (not stubs)
2. Zero-overhead calls (direct WAMR linkage)
3. Standard WASI modules (portable)
4. Reuses existing patterns (host functions, value conversion)

**When to implement:**
- **Now:** If WASM imports are blocking a critical use case
- **Later:** If other features are higher priority (e.g., exception handling verification, put_arg opcodes)

**Next Steps if proceeding:**
1. Implement Phase 2 (host function + wrapper) - ~4-5 hours
2. Create QuickJS patch for import hook - ~3 hours
3. Test with example WASM module - ~2 hours
4. Document and add examples - ~1 hour

Total: ~10-11 hours remaining work.

## FAQ

**Q: Can multiple WASM modules share memory?**
A: Yes, with WAMR's shared memory feature (not implemented in Phase 1, but possible).

**Q: What about WASM imports (WASI)?**
A: User WASM modules can import WASI functions. WAMR handles this automatically.

**Q: Performance vs native C?**
A: WASM is ~1.5-2x slower than native C. Our overhead adds ~5-10% on top.

**Q: Can WASM modules call back into JS?**
A: Not in Phase 1. Would need to export JS functions as WASM imports (bidirectional bridge).

**Q: Memory limits?**
A: Each WASM module gets its own heap (32MB default, configurable).
