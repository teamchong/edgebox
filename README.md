# EdgeBox

**AOT optimizer for V8 JavaScript** — automatically compiles numeric JS kernels to WebAssembly for 2-9x speedups. Works with Node.js, Deno, Cloudflare Workers, and any V8 runtime.

![EdgeBox Architecture](diagram.svg)

## The Problem

V8's JIT compiler is excellent at object-heavy JavaScript (property access, closures, string manipulation), but **leaves performance on the table for compute-intensive numeric code**. Functions like cryptographic hashes, compression kernels, and recursive algorithms run 2-5x slower than they need to because V8's speculative JIT can't fully eliminate dynamic type checks and boxing overhead for tight numeric loops.

Meanwhile, WebAssembly runs numeric code at near-native speed, but the **JS↔WASM boundary** has historically been too expensive for fine-grained function calls. Array data must be copied between JS heap and WASM linear memory on every call.

## Why It Works: V8 TurboFan WASM Inlining

Starting in V8 v12.0 (Chrome 120, Node.js 22+), V8's TurboFan JIT compiler **inlines WebAssembly function calls directly into JavaScript** — the same way it inlines JS→JS calls. This feature is **enabled by default** in all modern V8 runtimes:

```
JavaScript:  result = __wasm.exports.fib(n)
                        │
                        ▼  V8 TurboFan detects WASM call
              Inlines WASM code into JS compilation unit
                        │
                        ▼  Result
              Single native code stream — no boundary overhead
```

This means a JS function that calls `__wasm.exports.fib(n)` compiles to nearly the same machine code as if `fib` were implemented natively. The call frame setup, argument marshaling, and calling convention overhead are **completely eliminated**.

EdgeBox exploits this: it compiles numeric JS functions to WASM exports, then rewrites the JS source with thin trampolines that V8 inlines away. The result is LLVM-optimized numeric code running at full speed inside V8's pipeline.

**Supported V8 runtimes:**
- **Node.js 22+** (V8 v12.4+)
- **Deno** (V8-based)
- **Cloudflare Workers** (workerd, V8-based)
- **Chrome 120+** (for browser workloads)
- Any V8 embedder with default flags

## The Solution: AOT+JIT Compilation

EdgeBox is a build-time compiler that analyzes your JavaScript, identifies pure numeric functions, compiles them to **standalone WebAssembly**, and rewrites the source with trampolines that V8 inlines at zero cost:

```
JavaScript Source
    ↓  QuickJS bytecode analysis
Automatic numeric tier detection (i32 / f64 / array)
    ↓  LLVM IR codegen → standalone .wasm
Pure numeric kernels compiled to WASM exports
    ↓  Source-to-source transform
JS with function bodies replaced by WASM call trampolines
    ↓  V8 TurboFan (Node.js / Deno / workerd / Chrome)
V8 inlines WASM calls into JS — zero boundary overhead
```

**Key innovations:**
- **Automatic detection**: Analyzes QuickJS bytecodes to identify pure numeric functions (no manual annotation needed)
- **Smart trampolines**: Recursive/cross-calling functions → WASM (deep stacks stay native); pure scalar → keep as JS (V8 already optimal)
- **Zero-copy WASM memory**: `__wasmArray()` allocates TypedArrays directly in WASM linear memory — trampolines detect `arr.buffer === __wbuf` and skip copy entirely
- **Array copy caching**: Read-only array arguments cached by reference identity, eliminating redundant copies in tight loops
- **Cross-function calls**: Two-pass compilation detects functions that call other WASM functions, keeping entire call chains in WASM
- **Real npm packages work**: Tested with pako (zlib compression) and tweetnacl (cryptography)

## Benchmarks

**Overall: 2.58x faster than Node.js** across 39 benchmarks (24 wins, 7 ties, 8 losses).

### Headline Results (AOT+JIT on Node.js v24)

| Benchmark | AOT+JIT | Node.js | Speedup |
|-----------|:-------:|:-------:|:-------:|
| loop (zero-copy) | 8 ms | 70 ms | **8.7x faster** |
| loop (array sum) | 9 ms | 77 ms | **8.5x faster** |
| arrayStats 100K | 98 ms | 815 ms | **8.3x faster** |
| fib(40) | 533 ms | 2,599 ms | **4.8x faster** |
| ackermann(3,10) | 669 ms | 2,995 ms | **4.4x faster** |
| fib(45) | 2,150 ms | 9,549 ms | **4.4x faster** |
| hanoi(25) | 50 ms | 202 ms | **4.0x faster** |
| adler32 10K | 29 ms | 110 ms | **3.7x faster** |
| magnitudes 100K | 2,095 ms | 7,268 ms | **3.4x faster** |
| dotProductNorm 100K | 275 ms | 844 ms | **3.0x faster** |
| euclideanDist 100K | 280 ms | 823 ms | **2.9x faster** |
| prefixSum 10K | 118 ms | 256 ms | **2.1x faster** |

### Where It Excels vs Where It Doesn't

| Code Pattern | Speedup | Why |
|--------------|:-------:|-----|
| Array iteration loops | **7-9x** | WASM eliminates bounds checks, V8 inlines the trampoline |
| Recursive functions (fib, ackermann) | **4-5x** | Entire call stack stays in WASM, zero JS overhead |
| Numeric kernels (hash, CRC, crypto) | **2-4x** | WASM eliminates type checks, array caching skips copies |
| Float64 compute (dot product, distance) | **3x** | LLVM optimizes f64 loops better than V8's speculative JIT |
| Pure integer bitwise (rotr, popcount) | ~same | V8 JIT already compiles these to native integer ops |
| Object-heavy code | No benefit | V8 JIT already optimal for property access and closures |

## Quick Start

```bash
# Build EdgeBox compiler
zig build cli

# Compile your JS → optimized output
./zig-out/bin/edgeboxc my-app.js

# Output:
#   zig-out/bin/my-app.js/my-app-worker.mjs        — JS module (with WASM trampolines)
#   zig-out/bin/my-app.js/my-app-standalone.wasm    — Standalone WASM (AOT numeric kernels)

# Run on Node.js
node zig-out/bin/my-app.js/my-app-worker.mjs

# Run benchmarks
bash bench/run_all.sh
```

### Zero-Copy Arrays

For maximum performance, allocate arrays directly in WASM memory:

```javascript
// In your source code — check if __wasmArray is available (AOT+JIT path)
var data;
if (typeof __wasmArray === 'function') {
    data = __wasmArray(Int32Array, 100000);  // Allocated in WASM memory
} else {
    data = new Int32Array(100000);            // Normal allocation
}
// When data lives in WASM memory, trampolines skip copy entirely
result = sumArray(data);  // Zero overhead — direct WASM pointer
```

## How It Works

### 1. Bytecode Analysis

EdgeBox parses JavaScript with QuickJS-NG, then analyzes each function's bytecodes to determine if it's pure numeric:

| Tier | Detection | WASM Signature | Examples |
|------|-----------|----------------|----------|
| **i32** | Only arithmetic, bitwise, comparisons, locals, control flow | `(i32, i32) → i32` | `fib`, `gcd`, `isPrime` |
| **f64** | Uses division without bitwise (float semantics) | `(f64, f64) → f64` | `mandelbrot`, `lerp` |
| **i32+array** | Pure numeric + array element access | `(i32, i32_ptr, i32) → i32` | `adler32`, `crc32` |
| **f64+array** | Float numeric + array access | `(f64, f64_ptr, f64) → f64` | `dotProductNorm`, `euclideanDist` |

Functions that use objects, strings, closures, or other non-numeric patterns are left as JavaScript for V8's JIT.

### 2. LLVM Codegen → Standalone WASM

Numeric functions are compiled through LLVM to a standalone `.wasm` binary:
- **Cross-function calls resolved**: Two-pass compilation with forward declarations
- **Constant pool**: QuickJS `push_const` values resolved at compile time
- **Math intrinsics**: `Math.sqrt`, `Math.abs`, `Math.floor` → WASM f64 intrinsics
- **Up to 24 parameters**: Supports functions with many scalar arguments
- **O2 optimization**: LLVM auto-vectorization for float loops

### 3. Source-to-Source Transform

The original JavaScript is rewritten with function bodies replaced by WASM call trampolines:

```javascript
// Original
function adler32(adler, buf, len, pos) {
  /* 30 lines of bit manipulation */
}

// Transformed (auto-generated)
function adler32(adler, buf, len, pos) {
  if (buf.buffer === __wbuf) return __wasm.exports.adler32(adler, buf.byteOffset, len, pos);
  let __off = 0;
  const __b1 = __off;
  if (__last_fn !== 0 || buf.length < 128 || buf !== __c0_1) {
    __m.set(buf, __off); __c0_1 = buf;
  }
  __off += buf.length;
  __last_fn = 0;
  return __wasm.exports.adler32(adler, __b1 << 2, len, pos);
}
```

V8 TurboFan sees the `__wasm.exports.adler32(...)` call and inlines the WASM code directly — the trampoline's copy-or-skip logic and the WASM compute kernel compile into a single native code stream.

### 4. Smart Trampoline Decisions

| Function Pattern | Decision | Rationale |
|-----------------|----------|-----------|
| Recursive (calls itself) | **WASM trampoline** | Entire call stack in WASM, no JS↔WASM boundary per recursion |
| Cross-caller (calls other WASM functions) | **WASM trampoline** | Inter-function calls stay in WASM |
| Array + loop + compute | **WASM trampoline + cache** | Identity cache amortizes copy cost |
| Write-only heavy (loop ≥50 instrs) | **WASM trampoline** | No copy-in needed, only copy-back |
| Offset-indexed multi-array (≥2 arrays + ≥2 scalars) | **Keep as JS** | Offset-based access into small fixed regions, copy dominates |
| Write-only light (<50 instrs) | **Keep as JS** | V8 handles write loops perfectly |
| Pure scalar, non-recursive (<150 instrs) | **Keep as JS** | V8 JIT inlines these perfectly |

## Build Requirements

- **Zig 0.15+** — Build system
- **LLVM 20** — Only system dependency (for WASM codegen)

```bash
# macOS
brew install zig llvm@20

# Ubuntu/Debian
curl -fsSL https://ziglang.org/download/0.15.2/zig-linux-x86_64-0.15.2.tar.xz | tar -xJ
sudo apt-get install llvm-20 llvm-20-dev

# Build
zig build cli
```

## License

Apache License 2.0

**Vendored dependencies:**
- QuickJS-NG: MIT License
