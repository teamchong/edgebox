# CI Status - Node.js API Tests

## Latest Fixes (Dec 20, 2025)

### ✅ Test Runner Fixed (Commit 045aef8)
- Reverted to WASM mode (AOT has SIGSEGV issues)
- Test runner now prefers `.wasm` over `.aot` files
- Local testing shows 100% pass rate on buffer module

### ✅ Stack Size Improvements (Commit a3dbfdc)  
- EdgeBox runner: 8MB native stack
- EdgeBoxC compiler: 64MB native stack
- Prevents potential stack overflows in deep recursion

## Expected CI Results

Next CI run should show:
- **EdgeBox (WASM)**: ~100% pass rate
- **Bun**: ~100% pass rate  
- **Node.js**: ~100% pass rate

Previous 0% results were from commit 73f9391 which used broken AOT mode.

## AOT Investigation Status

AOT SIGSEGV issue remains unresolved:
- Crash at address 0x10 (NULL + 16 bytes)
- Affects all tests, even `print("Hello")`
- Likely WAMR AOT runtime issue with AArch64
- Not a frozen code generation bug (generated C is correct)
- Not a stack overflow (crashes even with 64MB stack)

Workaround: Use WASM interpreter mode (current default).
