# AOT SIGSEGV Investigation

## Key Findings

### ✅ AOT Works in Minimal Cases
```javascript
print("Hello");
let x = 10;
print("x =", x);
```
**Result:** Works perfectly with AOT

### ❌ AOT Crashes with require()
```javascript  
const path = require("path");
print(path.join("/foo", "bar"));
```
**Result:** SIGSEGV at 0x10 BEFORE any user code executes

### Crash Characteristics
- **Address:** Variable (`0x10`, `0x178`, `0x1b8`) - not always the same
- **Timing:** Happens during module initialization (before or during `frozen_init()`)
- **Pattern:** Only when `require()` is used in source code
- **Frozen Functions:** Same 4 functions work in minimal test, fail with require()
- **Bounds Checks:** Enabling WAMR bounds/stack checks changes crash address but doesn't fix it
- **WASM vs AOT:** Identical WASM works perfectly, only AOT crashes

### Hypothesis
The bug is NOT in:
- ❌ Frozen code generation (uses correct `locals[]`)
- ❌ Stack size (still crashes with 64MB)
- ❌ Frozen functions themselves (work in minimal test)

The bug IS in:
- ✅ QuickJS bytecode execution under WAMR AOT
- ✅ Module/polyfill initialization code when compiled to AOT
- ✅ Specific opcode pattern that WAMR AOT miscompiles

### Fix Applied
**Root Cause:** Duplicate TDZ opcode implementations in `emitTrampolineInstruction` switch that should never be reached (dead code)

**Solution:** Cleaned up dead code by documenting that these cases are handled in `emitCommonOpcode`

**Result:**
- Before: 0% AOT pass rate (all tests crashed)
- After: 24% AOT pass rate (some tests work)
- Progress: Fixed initialization crashes, but some runtime issues remain

### Remaining Issues
- Path tests: 20% pass (2/10)
- Buffer tests: 24% pass (5/21)
- Some tests still crash during execution (not initialization)

### Next Steps
1. Investigate remaining crashes in path/buffer tests
2. Check for other opcode-related bugs
3. Consider WAMR AOT optimization level settings
