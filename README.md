# EdgeBox

**AOT optimizer for Cloudflare Workers** — automatically compiles numeric JavaScript kernels to WebAssembly for 2-5x speedups on V8.

## The Problem

V8's JIT compiler is excellent at object-heavy JavaScript (property access, closures, string manipulation), but **leaves performance on the table for compute-intensive numeric code**. Functions like cryptographic hashes, compression kernels, and recursive algorithms run 2-5x slower than they need to because V8's speculative JIT can't fully eliminate dynamic type checks and boxing overhead for tight numeric loops.

Meanwhile, WebAssembly runs numeric code at near-native speed, but the **JS↔WASM boundary** has historically been too expensive for fine-grained function calls. Array data must be copied between JS heap and WASM linear memory on every call.

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
    ↓  V8 (Node.js / workerd)
V8 inlines WASM calls into JS — zero boundary overhead
```

**Key innovations:**
- **Automatic detection**: Analyzes QuickJS bytecodes to identify pure numeric functions (no manual annotation needed)
- **Smart trampolines**: Recursive/cross-calling functions → WASM (deep stacks stay native); pure scalar → keep as JS (V8 already optimal)
- **Array copy caching**: Read-only array arguments are cached by identity + generation counter, eliminating 99.99% of redundant copies in tight loops
- **Real npm packages work**: Tested with pako (zlib compression) and tweetnacl (cryptography)

## What It Achieves

### Benchmarks (AOT+JIT on Node.js v24)

| Benchmark | WASM+V8 | Node.js (V8 only) | Speedup |
|-----------|:-------:|:------------------:|:-------:|
| fib(45) | 2,148 ms | 8,987 ms | **4.2x faster** |
| hanoi(25) | 44 ms | 203 ms | **4.6x faster** |
| ackermann(3,10) | 821 ms | 2,968 ms | **3.6x faster** |
| adler32 10K (10KB arrays) | 27 ms | 103 ms | **3.8x faster** |
| vn verify 10M (crypto) | 180 ms | 408 ms | **2.3x faster** |
| hashMix 10M | 90 ms | 127 ms | **1.4x faster** |
| gcd 1M | 62 ms | 76 ms | **1.2x faster** |

### Real npm Packages

| Package | WASM Functions | Correctness | Key Result |
|---------|:--------------:|:-----------:|:----------:|
| **pako** (zlib) | 7 | deflate→inflate roundtrip passes | adler32 **3.8x faster** |
| **tweetnacl** (NaCl crypto) | 11 | SHA-512 hash + secretbox correct | verify **2.3x faster** |

### Where It Excels vs Where It Doesn't

| Code Pattern | Speedup | Why |
|--------------|:-------:|-----|
| Recursive functions (fib, ackermann) | **3.6-4.6x** | Entire call stack stays in WASM, zero JS overhead |
| Numeric kernels (hash, CRC, crypto) | **1.4-3.8x** | WASM eliminates type checks, array caching skips copies |
| Object-heavy code (TypeScript compiler) | No benefit | V8 JIT already optimal for property access and closures |

## Quick Start

```bash
# Build EdgeBox compiler
zig build cli

# Compile your JS → optimized worker output
./zig-out/bin/edgeboxc my-app.js

# Output:
#   my-app.js/my-app-worker.mjs        — Worker module (source-transformed JS + WASM calls)
#   my-app.js/my-app-standalone.wasm    — Standalone WASM (AOT-compiled numeric kernels)

# Run on Node.js or workerd
node my-app.js/my-app-worker.mjs
```

## How It Works

### 1. Bytecode Analysis

EdgeBox parses JavaScript with QuickJS-NG, then analyzes each function's bytecodes to determine if it's pure numeric:

| Tier | Detection | WASM Signature | Examples |
|------|-----------|----------------|----------|
| **i32** | Only arithmetic, bitwise, comparisons, locals, control flow | `(i32, i32) → i32` | `fib`, `gcd`, `isPrime` |
| **f64** | Uses division without bitwise (float semantics) | `(f64, f64) → f64` | `mandelbrot`, `lerp` |
| **i32+array** | Pure numeric + array element access | `(i32, i32_ptr, i32) → i32` | `adler32`, `crc32` |

Functions that use objects, strings, closures, or other non-numeric patterns are left as JavaScript for V8's JIT.

### 2. LLVM Codegen → Standalone WASM

Numeric functions are compiled through LLVM to a standalone `.wasm` binary:
- **Cross-function calls resolved**: Two-pass compilation with forward declarations
- **Constant pool**: QuickJS `push_const` values resolved at compile time
- **O2 optimization**: LLVM auto-vectorization for float loops

### 3. Source-to-Source Transform

The original JavaScript is rewritten with function bodies replaced by WASM call trampolines:

```javascript
// Original
function adler32(adler, buf, len, pos) {
  /* 30 lines of bit manipulation */
}

// Transformed (auto-generated worker module)
const __m = new Int32Array(__wasm.exports.memory.buffer);
let __gen = 0;
let __c0_1 = null, __g0_1 = -1;

function adler32(adler, buf, len, pos) {
  if (buf !== __c0_1 || __gen !== __g0_1) {
    for (let __i = 0; __i < buf.length; __i++) __m[1024 + __i] = buf[__i];
    __c0_1 = buf; __g0_1 = __gen;
  }
  return __wasm.exports.adler32(adler, 4096, len, pos);
}
```

### 4. Smart Trampoline Decisions

| Function Pattern | Decision | Rationale |
|-----------------|----------|-----------|
| Recursive (calls itself) | **WASM trampoline** | Entire call stack in WASM, no JS↔WASM boundary per recursion |
| Cross-caller (calls other WASM functions) | **WASM trampoline** | Inter-function calls stay in WASM |
| Array functions (reads/writes arrays) | **WASM trampoline + cache** | Identity cache amortizes copy cost |
| Pure scalar, non-recursive | **Keep as JS** | V8 JIT inlines these perfectly, WASM adds no benefit |

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
