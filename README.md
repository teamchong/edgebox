# EdgeBox

QuickJS JavaScript runtime with **WAMR AOT compilation** for fast, sandboxed JavaScript execution at the edge.

## Architecture

```
┌───────────────────────────────────────────────────────────┐
│                       EdgeBox                             │
├───────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌───────────────────┐  │
│  │  QuickJS-NG │  │   WASI      │  │   WAMR Runtime    │  │
│  │  (ES2023)   │──│  (preview1) │──│   + AOT Compiler  │  │
│  │  [vendored] │  │             │  │   (LLVM-based)    │  │
│  └─────────────┘  └─────────────┘  └───────────────────┘  │
├───────────────────────────────────────────────────────────┤
│  ┌─────────────────────────────────────────────────────┐  │
│  │              Node.js Polyfills                      │  │
│  │  - Buffer, path, events, util, os, tty              │  │
│  │  - process.stdin/stdout/stderr, env, argv           │  │
│  │  - fetch (HTTP/HTTPS), child_process (spawnSync)    │  │
│  │  - TLS 1.3 (X25519 + AES-GCM via std.crypto)        │  │
│  └─────────────────────────────────────────────────────┘  │
├───────────────────────────────────────────────────────────┤
│  ┌─────────────────────────────────────────────────────┐  │
│  │           WAMR AOT (Ahead-of-Time) Compilation      │  │
│  │  - WASM → Native code at build time (via LLVM)      │  │
│  │  - Still sandboxed: memory bounds checks preserved  │  │
│  │  - WASI syscalls intercepted by runtime             │  │
│  │  - 13ms cold start, 454KB binary size               │  │
│  └─────────────────────────────────────────────────────┘  │
└───────────────────────────────────────────────────────────┘
```

## What is WAMR AOT?

**WAMR** (WebAssembly Micro Runtime) is a lightweight WASM runtime from the Bytecode Alliance. EdgeBox uses WAMR with **AOT (Ahead-of-Time) compilation** to achieve native performance while maintaining WASM's security guarantees.

### How AOT Works

```
Build Time (wamrc compiler):
  module.wasm  ──LLVM──>  module.aot
    │                        │
    │  WASM bytecode         │  Native machine code
    │  (portable)            │  (platform-specific)
    │                        │
    └── Validated & bounds   └── Bounds checks compiled in
        checks at parse          as native instructions
```

### AOT is Still Sandboxed

The `.aot` file contains native machine code, but it's **still sandboxed**:

1. **Memory Isolation**: All memory accesses go through bounds-checked instructions compiled into the AOT code. Out-of-bounds access triggers a trap, not a segfault.

2. **WASI Syscall Interception**: All I/O (filesystem, network, process) goes through WASI, which EdgeBox controls. You configure allowed directories in `.edgebox.json`.

3. **No Arbitrary Code Execution**: The AOT code is compiled from validated WASM - it can only do what WASM allows, just faster.

```
┌─────────────────────────────────────────────────────────┐
│                    Security Model                       │
├─────────────────────────────────────────────────────────┤
│  .aot file (native code)                                │
│    ├── Memory access: bounds-checked instructions       │
│    ├── Function calls: validated call table             │
│    └── System calls: all go through WASI →              │
│                                           ↓             │
│  WAMR Runtime                                           │
│    ├── WASI filesystem: only dirs in .edgebox.json      │
│    ├── WASI sockets: controlled by runtime              │
│    └── Process spawning: command allowlist              │
└─────────────────────────────────────────────────────────┘
```

**Think of it as**: "Pre-compiled WASM" - same sandbox, native speed.

## Build Requirements

- **Zig 0.13+** - Build system
- **LLVM 18** - Required for wamrc (AOT compiler only)
- **Bun** - For bundling JS files

```bash
# macOS
brew install zig llvm@18 oven-sh/bun/bun

# Build wamrc (AOT compiler) from WAMR source
cd vendor/wamr/wamr-compiler
mkdir -p build && cd build
cmake .. -DLLVM_DIR=/opt/homebrew/opt/llvm@18/lib/cmake/llvm
make -j8
cp wamrc ../../../../zig-out/bin/
```

### ARM64 Mac (Apple Silicon) Support

EdgeBox fully supports ARM64 Mac with these execution modes:

| Mode | Binary | Performance | Notes |
|------|--------|-------------|-------|
| **AOT** | `edgebox file.aot` | 100% native | **Recommended** - compile once with wamrc |
| **Fast JIT** | `edgebox-rosetta file.wasm` | ~50% native | Auto-built, runs x86_64 via Rosetta 2 |
| **Interpreter** | `edgebox file.wasm` | ~5% native | Fallback, always works |

**Why no native ARM64 JIT?**
- WAMR's Fast JIT uses `asmjit` library which only supports x86_64
- WAMR's LLVM JIT supports ARM64 but requires linking ~1.8GB LLVM libs
- Rosetta 2 is a pragmatic solution: run x86_64 Fast JIT with ~95% translation efficiency

**Recommendation:** Use AOT for production (best performance, smallest binary).

## Quick Start

```bash
# 1. Build EdgeBox CLI tools
zig build cli -Doptimize=ReleaseFast

# 2. Build your JS app to AOT (frozen functions + native code)
./zig-out/bin/edgeboxc build myapp.js
# Creates: edgebox-static.aot (fast!)

# 3. Run
./zig-out/bin/edgebox edgebox-static.aot
```

## Performance

Benchmarks run on WAMR (WebAssembly Micro Runtime) with **AOT compilation** and **Fast JIT** (via Rosetta 2 on ARM64 Mac). Tests 6 runtimes across 3 benchmarks.

### Cold Start (hello.js)

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `Bun (CLI)` | 20.2 ± 1.2 | 18.2 | 23.2 | **1.00** |
| `EdgeBox (daemon)` | 27.5 ± 4.2 | 21.5 | 39.2 | 1.36x |
| `Node.js (CLI)` | 37.9 ± 1.3 | 34.6 | 40.2 | 1.88x |
| `Porffor (porf <js>)` | 47.6 ± 5.4 | 42.0 | 67.8 | 2.36x |
| `EdgeBox (AOT)` | 50.8 ± 8.9 | 44.1 | 81.9 | 2.52x |
| `EdgeBox (WASM)` | 318.0 ± 197.5 | 239.5 | 1138.2 | 15.76x |

> **Note:** EdgeBox daemon uses a pre-allocated pool of warm WASM instances. WASM benchmarks on ARM64 Mac use Fast JIT via Rosetta 2 (`edgebox-rosetta`).

### Memory Usage (600k objects - peak RSS)

| Runtime | Peak Memory | Relative |
|:---|---:|---:|
| `Bun` | 120.0 MB | **1.00** |
| `Node.js` | 140.2 MB | 1.17x |
| `EdgeBox (AOT)` | 348.7 MB | 2.91x |
| `EdgeBox (WASM)` | 348.7 MB | 2.91x |
| `Porffor (porf <js>)` | 1253.5 MB | 10.45x |

> EdgeBox uses an arena allocator optimized for request-response patterns. Higher peak memory is a trade-off for O(1) allocation and instant cleanup between requests.

### CPU fib(45) - Frozen Interpreter Benchmark

| Runtime | Computation Time | Relative |
|:---|---:|---:|
| `EdgeBox (AOT)` | 2885.92 ms | **1.00** |
| `Bun` | 5305.33 ms | 1.84x |
| `Node.js` | 7736.47 ms | 2.68x |
| `EdgeBox (WASM)` | 8130.92 ms | 2.82x |
| `Porffor (porf <js>)` | 9203.82 ms | 3.19x |

> All results validated: `fib(45) = 1134903170` ✓
> Benchmark uses `performance.now()` for pure computation time (excludes startup).
> WASM benchmarks on ARM64 Mac use Fast JIT via Rosetta 2 (`edgebox-rosetta`).

**EdgeBox is 1.84x faster than Bun** and **2.69x faster than Node.js** on pure computation.

The frozen interpreter transpiles recursive JS to native C code, eliminating JSValue boxing overhead in tight loops.

### How the Frozen Interpreter Works

The **frozen interpreter** transpiles **ALL pure JS functions** to native C code:

1. **Detection**: Scans bytecode - any function using only supported opcodes gets frozen
2. **Code generation**: Unrolls bytecode to direct C with SMI (Small Integer) fast paths
3. **Hook injection**: Adds `if(globalThis.__frozen_X) return __frozen_X(...)` to JS
4. **AOT compilation**: WAMR compiles WASM+frozen C to native machine code
5. **Self-recursion optimization**: Recursive functions get direct C calls (no JS overhead)

```c
// Generated frozen function - works for ANY pure function
static JSValue frozen_square(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    JSValue stack[256]; int sp = 0;
    /* get_arg0 */ PUSH(argc > 0 ? FROZEN_DUP(ctx, argv[0]) : JS_UNDEFINED);
    /* dup */      PUSH(FROZEN_DUP(ctx, TOP()));
    /* mul */      { JSValue b = POP(), a = POP(); PUSH(frozen_mul(ctx, a, b)); }
    return POP();
}

// Self-recursive functions get additional optimization (direct C recursion)
static int64_t frozen_fib_native(int64_t n) {
    if (n < 2) return n;
    return frozen_fib_native(n - 1) + frozen_fib_native(n - 2);
}
```

**Why it's safe:**
- Only optimizes pure functions (no side effects, no closures)
- SMI fast path for int32 arithmetic (no heap allocation)
- Falls back to JS interpreter for unsupported opcodes

**Integrated into edgeboxc build pipeline:**
```bash
# Build automatically freezes ALL pure functions
zig build cli -Doptimize=ReleaseFast
./zig-out/bin/edgeboxc build myapp/

# Manual usage
./zig-out/bin/qjsc -e -o bytecode.c -N mymodule myfunc.js
./zig-out/bin/edgebox-freeze bytecode.c -o frozen.c -m mymodule
```

**Architecture:** Function-level validation ensures correctness. If a function contains unsupported opcodes, it runs in the interpreter instead - no crashes, no wrong results.

**Supported opcodes (87 of ~250):**

| Category | Count | Opcodes | Comptime |
|----------|-------|---------|----------|
| Arithmetic | 12 | **add, sub, mul, div, mod, inc, dec, neg, plus**, inc_loc, dec_loc, add_loc | 9 ✓ |
| Comparison | 8 | **lt, lte, gt, gte, eq, neq, strict_eq, strict_neq** | 8 ✓ |
| Bitwise | 7 | **and, or, xor, shl, sar, shr, not** | 7 ✓ |
| Push/const | 14 | **push_minus1, push_0..7, push_true, push_false, null, undefined**, push_i8/i16/i32 | 13 ✓ |
| Locals | 12 | **get_loc0..3**, get_loc, get_loc8, **put_loc0..3**, put_loc, put_loc8 | 8 ✓ |
| Arguments | 7 | **get_arg0..3**, get_arg, **put_arg0, put_arg1** | 6 ✓ |
| Var refs | 4 | get_var_ref0..3 | - |
| Control | 10 | if_false/8, if_true/8, goto/8/16, **return, return_undef** | 2 ✓ |
| Calls | 4 | call0, call1, call2, call3 | - |
| Stack | 3 | **drop, dup, dup2** | 3 ✓ |
| Property | 3 | **get_field, get_field2, put_field** | 3 ✓ |
| TCO | 2 | **tail_call, tail_call_method** | 2 ✓ |

> **Bold** = comptime-generated from `opcode_handlers.zig` patterns (61 ops)

**Supported:** Any pure function using only the above opcodes (arithmetic, comparison, locals, args, control flow, property access).
**Not supported:** Closures, async/await, classes, `eval`.

#### Comptime Architecture (Auto-sync with QuickJS-NG)

The frozen interpreter uses **Zig comptime** to auto-generate opcode handlers from patterns. When QuickJS-NG updates, handlers regenerate automatically:

```
vendor/quickjs-ng/quickjs-opcode.h    # QuickJS opcode definitions
        ↓ zig build gen-opcodes
src/freeze/opcodes_gen.zig            # Generated enum with opcode values
        ↓ comptime
src/freeze/opcode_handlers.zig        # Pattern mappings (name → handler)
        ↓ comptime generateCode()
Generated C code                       # Handlers auto-generated at compile time
```

**Why this works:**
- Opcode **values** may change between QuickJS versions (e.g., `add` = 45 → 47)
- Comptime patterns are tied to **names**, not values
- `getHandler(.add)` always maps to `binary_arith` pattern regardless of value

```zig
// opcode_handlers.zig - tied to NAME, not numeric value
.add => .{ .pattern = .binary_arith, .c_func = "frozen_add" },
.sub => .{ .pattern = .binary_arith, .c_func = "frozen_sub" },

// When QuickJS updates opcodes_gen.zig with new values,
// the pattern mapping stays the same - handlers auto-regenerate!
```

**After QuickJS-NG update:**
```bash
cd vendor/quickjs-ng && git pull    # Update QuickJS-NG
zig build gen-opcodes               # Regenerate opcodes_gen.zig from headers
zig build freeze                    # Comptime regenerates all 61 handlers
```

**Key Insights:**
- **EdgeBox daemon warm (~15ms)** is competitive with Bun for startup when pool is ready
- **EdgeBox AOT** is **1.84x faster than Bun**, **2.69x faster than Node.js** on CPU-bound tasks
- **EdgeBox is sandboxed** via WASM - memory bounds checks + WASI syscall interception
- **CPU-bound tasks**: Frozen interpreter delivers native C performance
- Binary size: **454KB** (minimal WAMR runtime)

**Trade-offs:**
- Startup overhead (~40ms for WAMR AOT loading) is higher than Bun/Node.js (~10-20ms)
- Best for: CPU-intensive workloads, sandboxed execution, edge deployment
- For short-lived tasks: Use daemon mode with warm pool for fast startup

### Memory Allocator Strategy

EdgeBox uses **different allocators** depending on the binary:

| Binary | Allocator | Why |
|--------|-----------|-----|
| `edgeboxd` (daemon) | Bump/Arena + Reset | O(1) alloc, instant cleanup between requests |
| `edgebox` (CLI) | mimalloc/jemalloc | Proper free(), lower peak memory for long-running |

#### edgeboxd: Smart Arena Allocator

Optimized for serverless/request-response patterns:

| Operation | Speed | How |
|-----------|-------|-----|
| malloc | O(1) | Bump pointer with size header |
| realloc (grow) | O(1)* | In-place if at top of arena |
| free | O(1)* | Reclaim if at top (LIFO pattern) |
| reset | O(1) | Reset pointer to mark position |

*\*LIFO optimization - hits ~80% of QuickJS allocations*

```
Request 1: alloc → alloc → alloc → reset() // Instant cleanup
Request 2: alloc → alloc → alloc → reset() // Reuses same memory
```

**Trade-off**: Higher peak memory (free is no-op for non-LIFO), but faster allocation and zero fragmentation.

#### edgebox: Standard Allocator

For long-running CLI processes, uses mimalloc/jemalloc for proper memory reclamation:
- Real `free()` returns memory to OS
- Lower peak memory usage
- Better for interactive/REPL usage

### Daemon Mode (Batch Instance Pool)

EdgeBox includes a high-performance HTTP daemon (`edgeboxd`) that uses a **batch instance pool** architecture for near-zero request latency:

```
┌─────────────────────────────────────────────────────────────────┐
│                         edgeboxd                                │
├─────────────────────────────────────────────────────────────────┤
│  ┌───────────────────┐                                          │
│  │  Pool Manager     │  Background thread continuously          │
│  │  Thread           │  pre-instantiates WASM instances         │
│  └─────────┬─────────┘                                          │
│            │ fills                                              │
│            ▼                                                    │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │              Instance Pool (Ring Buffer)                │    │
│  │  ┌────┐ ┌────┐ ┌────┐ ┌────┐ ┌────┐ ┌────┐ ┌────┐       │    │
│  │  │Inst│ │Inst│ │Inst│ │Inst│ │Inst│ │Inst│ │Inst│ ...   │    │
│  │  │ 1  │ │ 2  │ │ 3  │ │ 4  │ │ 5  │ │ 6  │ │ 7  │       │    │
│  │  └────┘ └────┘ └────┘ └────┘ └────┘ └────┘ └────┘       │    │
│  │    ↑                                                    │    │
│  │   grab                                                  │    │
│  └─────────────────────────────────────────────────────────┘    │
│            ↑                                                    │
│  ┌─────────┴─────────┐                                          │
│  │  HTTP Server      │  Grabs ready instance (0ms!)             │
│  │  (main thread)    │  Executes → Destroys → Returns           │
│  └───────────────────┘                                          │
└─────────────────────────────────────────────────────────────────┘
```

**Key insight:** Instead of instantiating WASM per-request (~2ms), we pre-instantiate a pool of instances in the background. Requests grab a ready instance instantly (0ms instantiation wait).

```bash
# Start daemon with 32 pre-instantiated instances
edgeboxd edgebox.wasm --pool-size=32 --port=8080

# Daemon performance:
# - Pool hit:  ~2-3ms total (grab ready instance + execute)
# - Pool miss: ~4-5ms total (fallback to on-demand instantiation)
# - Pool pre-fill: 32 instances in ~50ms at startup
```

**Configuration in `.edgebox.json`:**
```json
{
  "daemon": {
    "pool_size": 32,           // Pre-instantiated instances per batch
    "exec_timeout_ms": 30000,  // Max execution time per request
    "port": 8080
  }
}
```

**How it works:**
1. **Startup:** Module loaded once, pool pre-filled with N instances
2. **Request:** Grab ready instance from pool → execute → destroy
3. **Background:** Pool manager thread continuously replenishes pool
4. **Graceful degradation:** If pool empty (burst), fall back to on-demand instantiation

This is similar to how **Cloudflare Workers** maintains a warm pool of isolates, but using WASM instances instead of V8 isolates.

### vs Anthropic sandbox-runtime

EdgeBox takes a different approach to sandboxing compared to [Anthropic's sandbox-runtime](https://github.com/anthropic-experimental/sandbox-runtime):

| Aspect | **EdgeBox** | **sandbox-runtime** |
|--------|-------------|---------------------|
| **Approach** | WASM sandbox (code runs inside) | OS-level sandbox (wraps process) |
| **Technology** | WAMR AOT + QuickJS | macOS: `sandbox-exec`, Linux: `bubblewrap` |
| **Cold Start** | **13ms** | ~50-200ms |
| **Memory** | **~2MB** | ~50MB+ |
| **Binary Size** | **454KB** | N/A (uses system tools) |
| **Can Run** | JavaScript only | Any binary (git, python, etc.) |
| **Network** | Built-in fetch (via host) | HTTP/SOCKS5 proxy filtering |
| **Command Control** | Argument-level allowlist | N/A (wraps any process) |
| **Windows** | Yes (WASM portable) | No |

**When to use EdgeBox**: Running untrusted JS at edge/serverless scale with minimal overhead.

**When to use sandbox-runtime**: Sandboxing arbitrary binaries on developer machines.

**Complementary**: EdgeBox can use sandbox-runtime to wrap `child_process.spawnSync()` calls for defense-in-depth.

### OS-Level Process Sandbox

EdgeBox includes an OS-level sandbox wrapper (`edgebox-sandbox`) that enforces `.edgebox.json` `dirs` permissions when spawning child processes:

| Platform | Technology | Status |
|----------|------------|--------|
| macOS | `sandbox-exec` | ✅ |
| Linux | `bubblewrap` | ✅ |
| Windows | Job Objects + Restricted Tokens | ✅ |

This prevents shell escape attacks like `git checkout > /etc/passwd` by restricting filesystem access at the kernel level. The sandbox is automatic - when `dirs` is configured, all `child_process` commands are wrapped.

### Optimizations

1. **WAMR AOT Compilation** - LLVM compiles WASM → native code at build time
2. **Lightweight Runtime** - WAMR is 465KB binary
3. **Memory Bounds Checks** - Compiled into native code (no runtime overhead)
4. **WASM SIMD128** - 16-byte vector operations for string processing
5. **Bump Allocator** - O(1) malloc (pointer bump), NO-OP free (memory reclaimed at exit)
6. **wasm-opt -Oz** - 82% binary size reduction (5.8MB → 1.6MB WASM)
7. **Lazy Polyfills** - Only inject minimal bootstrap on startup, load Node.js polyfills on-demand
8. **Bytecode Caching** - Pre-compile JavaScript at build time, skip parsing at runtime

### qjsc Bytecode Compilation

EdgeBox uses `qjsc` (QuickJS compiler) to pre-compile JavaScript to bytecode at build time:

```
edgebox build my-app/
  ↓
bundle.js (12KB)         → JavaScript source + polyfills
  ↓ qjsc
bundle_compiled.c (71KB) → Bytecode embedded as C array
  ↓ zig build
edgebox-static.wasm      → WASM with bytecode baked in

edgebox run edgebox-static.wasm
  ↓
Load bytecode directly → Execute (no JS parsing)
```

This eliminates all JavaScript parsing at runtime - the bytecode is part of the WASM binary.

### Wizer Pre-initialization

EdgeBox includes a **pure Zig implementation** of [Wizer](https://github.com/bytecodealliance/wizer) (`src/wizer.zig`) to pre-initialize the QuickJS runtime at build time. This removes the Rust dependency and integrates directly into the build pipeline.

```
Build time (wizer_init):
  ├── JS_NewRuntime()      # Create QuickJS runtime
  ├── JS_NewContext()      # Create context
  ├── js_init_module_std() # Initialize std module
  ├── initStaticPolyfills() # TextEncoder, URL, Event, etc.
  └── Snapshot memory      # Embedded in WASM binary

Runtime (_start):
  ├── Check wizer_initialized flag
  ├── js_std_add_helpers() # Bind console/print
  ├── bindDynamicState()   # process.argv, process.env
  └── Execute user code    # 0.03ms to first instruction
```

The Zig Wizer implementation:
- Loads WASM via WAMR C API
- Runs `wizer.initialize` export to execute init code
- Snapshots memory with sparse segment optimization (merges gaps < 4 bytes)
- Rewrites WASM binary with pre-initialized data segments

```bash
# Manual snapshot (usually handled by edgeboxc build)
edgeboxc snapshot input.wasm output.wasm --init-func=wizer.initialize
```

### Prerequisites

1. **Zig 0.13+** (required for building):
```bash
# macOS
brew install zig

# Linux: https://ziglang.org/download/
```

2. **LLVM 18** (optional, for AOT compilation):
```bash
# macOS
brew install llvm@18

# Only needed to build wamrc (AOT compiler)
# Pre-compiled .aot files don't need LLVM
```

3. **Bun** (optional, for JS bundling):
```bash
# macOS
brew install oven-sh/bun/bun

# Linux/macOS (alternative)
curl -fsSL https://bun.sh/install | bash
```

### Build & Run

```bash
# Build WAMR runtime (recommended)
zig build wamr -Doptimize=ReleaseFast

# Build WASM module
zig build wasm -Doptimize=ReleaseFast

# AOT compile WASM → native (requires wamrc)
./vendor/wamr/wamr-compiler/build/wamrc -o edgebox.aot zig-out/bin/edgebox-base.wasm

# Run with AOT (fastest - 10ms cold start)
./zig-out/bin/edgebox-wamr edgebox.aot hello.js

# Run with interpreter (no AOT needed - 48ms cold start)
./zig-out/bin/edgebox-wamr zig-out/bin/edgebox-base.wasm hello.js
```

### Building the AOT Compiler (wamrc)

**LLVM is required only for building `wamrc`** (the AOT compiler). Once you have an `.aot` file, LLVM is NOT needed to run it.

```bash
# 1. Install LLVM 18 (one-time setup)
brew install llvm@18   # macOS
# apt install llvm-18  # Linux

# 2. Build wamrc
cd vendor/wamr/wamr-compiler
mkdir -p build && cd build
cmake .. -DWAMR_BUILD_WITH_CUSTOM_LLVM=1 -DCMAKE_PREFIX_PATH="/opt/homebrew/opt/llvm@18"
make -j4

# 3. AOT compile your WASM
./wamrc -o edgebox.aot edgebox-base.wasm

# 4. Run (no LLVM needed!)
./edgebox-wamr edgebox.aot hello.js
```

**Note**: We don't bundle LLVM (it's 1.8GB). Users either:
1. Install LLVM and build `wamrc` themselves, OR
2. Use pre-compiled `.aot` files (we can ship these), OR
3. Run in interpreter mode (no AOT, 48ms instead of 10ms)

### CLI Tools

EdgeBox provides these binaries:

| Binary | Purpose | Cold Start |
|--------|---------|------------|
| `edgebox` | WAMR runtime (AOT or interpreter) | **10ms** (AOT) / 48ms (interp) |
| `edgeboxd` | HTTP daemon with batch instance pool | ~2ms per request (warm) |
| `edgeboxc` | Build tools (bundle, compile) | N/A |

#### edgebox-wamr - Fast WAMR Runner (Recommended)
```bash
# Run with AOT (fastest)
edgebox-wamr edgebox.aot hello.js

# Run with interpreter (no AOT compilation needed)
edgebox-wamr edgebox.wasm hello.js
```

#### edgeboxd - HTTP Daemon (Batch Pool)
```bash
# Start daemon with default settings (pool_size=32)
edgeboxd edgebox.wasm

# Custom pool size and port
edgeboxd edgebox.wasm --pool-size=64 --port=9000

# With execution timeout
edgeboxd edgebox.wasm --timeout=5000

# Test request
curl http://localhost:8080/
```

The daemon reads config from `.edgebox.json` and CLI args override the config file.

#### edgeboxc - Build CLI
```bash
edgeboxc build [app-directory]   # Compile JS to WASM with embedded bytecode
edgeboxc build my-app            # Example
```

### Build Pipeline

```
zig build wasm
  ↓
edgebox-base.wasm (1.6MB) → QuickJS + polyfills as WASM
  ↓ wamrc (LLVM AOT compiler)
edgebox.aot (2.6MB)       → Native machine code (still sandboxed!)
  ↓
edgebox-wamr edgebox.aot hello.js → 10ms cold start
```

The AOT file is native code but still runs in the WASM sandbox - all memory accesses are bounds-checked and all I/O goes through WASI.

## App Configuration

Apps can include a `.edgebox.json` config file:

```json
{
  "name": "my-app",
  "npm": "@anthropic-ai/claude-code",
  "runtime": {
    "stack_size": 16777216,
    "heap_size": 268435456,
    "max_memory_pages": 32768,
    "max_instructions": -1
  },
  "dirs": [
    {"path": "/", "read": true, "write": false, "execute": true},
    {"path": "/tmp", "read": true, "write": true, "execute": true},
    {"path": "$HOME", "read": true, "write": true, "execute": true}
  ],
  "env": ["ANTHROPIC_API_KEY", "HOME", "USER", "PATH"],
  "allowCommands": ["git", "npm", "node", "curl", "cat", "ls"],
  "denyCommands": ["sudo", "su", "rm"],
  "useKeychain": true
}
```

| Field | Description |
|-------|-------------|
| `npm` | npm package to install and use as entry point |
| `runtime` | Runtime memory configuration: `stack_size`, `heap_size`, `max_memory_pages` |
| `dirs` | Directory permissions (see below) |
| `env` | Environment variables to pass to the app |
| `allowCommands` | Commands allowed for spawn (empty = allow all) |
| `denyCommands` | Commands denied for spawn (takes precedence) |
| `allowedUrls` | URL patterns allowed for fetch (glob: `https://api.anthropic.com/*`) |
| `blockedUrls` | URL patterns blocked (takes precedence over allowed) |
| `useKeychain` | Read API key from macOS keychain (default: `false`) |
| `rateLimitRps` | Max HTTP requests per second (default: `0` = unlimited) |
| `maxConnections` | Max concurrent HTTP connections (default: `100`) |

### Runtime Configuration (runtime)

Control WASM runtime memory limits for CPU and memory-intensive applications.

```json
{
  "runtime": {
    "stack_size": 16777216,
    "heap_size": 268435456,
    "max_memory_pages": 32768,
    "max_instructions": -1
  }
}
```

| Field | Default | Description |
|-------|---------|-------------|
| `stack_size` | `8388608` (8MB) | WASM execution stack size in bytes |
| `heap_size` | `268435456` (256MB) | Host-managed heap for WAMR runtime allocations |
| `max_memory_pages` | `32768` (2GB) | Maximum WASM linear memory pages (64KB each) |
| `max_instructions` | `-1` (unlimited) | CPU limit: max instructions before termination (interpreter only) |

**Memory model explained:**

```
┌─────────────────────────────────────────────────────────────┐
│                    WASM Memory Model                         │
├─────────────────────────────────────────────────────────────┤
│  heap_size (host-managed)                                   │
│  └── WAMR internal structures, module metadata              │
│                                                             │
│  max_memory_pages × 64KB (WASM linear memory)              │
│  └── QuickJS heap, JS objects, bytecode, user data         │
│  └── Can grow dynamically up to max_memory_pages limit     │
│  └── WASM32 maximum: 65536 pages = 4GB                     │
└─────────────────────────────────────────────────────────────┘
```

**Key difference:**
- `heap_size`: Fixed allocation for WAMR's internal use
- `max_memory_pages`: Cap on how much WASM can dynamically allocate via `memory.grow`

**Common configurations:**

| Use Case | stack_size | heap_size | max_memory_pages | Notes |
|----------|------------|-----------|------------------|-------|
| Simple scripts | 8MB | 256MB | 32768 (2GB) | Default, sufficient for most apps |
| Large bundles (>10MB bytecode) | 16MB | 256MB | 32768 (2GB) | Claude CLI, complex apps |
| Memory-intensive (large objects) | 16MB | 512MB | 65536 (4GB) | Data processing, large JSON |

**Example for large applications:**
```json
{
  "runtime": {
    "stack_size": 16777216,
    "heap_size": 268435456,
    "max_memory_pages": 32768
  }
}
```

**Debugging memory issues:**
```bash
# Enable debug output to see actual memory settings
EDGEBOX_DEBUG=1 ./zig-out/bin/edgebox app.aot

# Output shows:
# [DEBUG] Instantiating module (stack=16MB, heap=256MB, max_memory=2048MB)...
```

**Notes:**
- `max_memory_pages` controls how much memory WASM can dynamically request
- Each page is 64KB, so 32768 pages = 2GB, 65536 pages = 4GB (WASM32 max)
- For 40MB+ bytecode (like Claude CLI), the default 2GB max is usually sufficient
- Stack overflow manifests as "out of bounds memory access" errors
- Environment variable `$HOME` and `$PWD` are expanded in `dirs` paths

**CPU Limits (`max_instructions`):**
- Only works in **interpreter mode** (`.wasm` files), not AOT mode (`.aot` files)
- Set to a positive integer to limit CPU time (e.g., `1000000000` for ~1B instructions)
- When exceeded, execution terminates with "instruction limit exceeded" error
- AOT mode ignores this setting (no overhead) - use OS-level timeout instead
- Useful for sandboxing untrusted code in development/testing

### Directory Permissions (dirs)

Control filesystem and shell access per directory. **Default: no access**.

```json
{
  "dirs": [
    {"path": "/", "read": true, "write": false, "execute": true},
    {"path": "/tmp", "read": true, "write": true, "execute": true},
    {"path": "$HOME", "read": true, "write": true, "execute": true},
    {"path": "$PWD", "read": true, "write": true, "execute": true}
  ]
}
```

| Field | Type | Description |
|-------|------|-------------|
| `path` | string | Directory path (supports `$HOME`, `$PWD`, `~`) |
| `read` | boolean | Allow reading files in directory |
| `write` | boolean | Allow writing/creating files in directory |
| `execute` | boolean | Allow spawning commands (child_process) |

**Security notes:**
- By default, **no directories are accessible** (must explicitly grant)
- Shell access requires `execute: true` on at least one directory
- Use `allowCommands`/`denyCommands` for additional command filtering
- Supports `$HOME`, `$PWD`, and `~` for path expansion
- `/home/user` paths are automatically mapped to actual `$HOME` on the host

### Command Filtering (allowCommands / denyCommands)

Control which commands can be spawned via child_process.

```json
{
  "allowCommands": ["git", "npm", "node", "curl"],
  "denyCommands": ["sudo", "su", "rm", "chmod"]
}
```

| Field | Description |
|-------|-------------|
| `allowCommands` | If set, only these commands can run (empty = allow all) |
| `denyCommands` | These commands are always blocked (takes precedence) |

**Security notes:**
- Commands are matched by basename (e.g., `/usr/bin/git` matches `git`)
- Deny list always takes precedence over allow list
- If allow list is empty, all commands are allowed (use deny list to block specific ones)
- Both require `x` permission in `dirs` - command filtering is an additional layer

### HTTP Security (Optional)

Control which URLs the app can fetch. **Default: permissive (no restrictions)**.

```json
{
  "allowedUrls": [
    "https://api.anthropic.com/*",
    "https://api.openai.com/*"
  ],
  "blockedUrls": [
    "https://internal.corp/*"
  ],
  "useKeychain": true
}
```

| Field | Default | Description |
|-------|---------|-------------|
| `allowedUrls` | `[]` (allow all) | Glob patterns for allowed URLs |
| `blockedUrls` | `[]` | Glob patterns to block (takes precedence) |
| `useKeychain` | `false` | Read `ANTHROPIC_API_KEY` from macOS keychain |
| `rateLimitRps` | `0` | Requests per second limit (0 = unlimited) |
| `maxConnections` | `100` | Max concurrent connections |

**Security notes:**
- WASM cannot access keychain directly - host reads it if `useKeychain: true`
- URL restrictions are enforced on the host side (WASM has no raw network access)
- Empty `allowedUrls` = allow all URLs (permissive by default)

### Command Permissions

The `commands` field provides fine-grained control over which system commands can be executed via `child_process.spawnSync()`:

```json
{
  "commands": {
    "git": ["clone", "status", "add", "commit"],
    "npm": ["install", "run"],
    "node": true,
    "curl": true
  }
}
```

| Value | Meaning |
|-------|---------|
| `["subcommand1", "subcommand2"]` | Only allow these subcommands (first argument must match) |
| `true` | Allow all arguments |
| Binary not listed | Denied (permission error) |

**Security Model:**
- **Default deny**: If `commands` is not specified, no commands can be executed
- **Explicit allowlist**: Only whitelisted binaries can run
- **Subcommand filtering**: For `git`, `npm`, etc., you can restrict to specific operations
- **No escape hatch**: Unlike containers, there's no way to bypass with shell tricks

**Example: Minimal permissions for a git-only workflow:**
```json
{
  "commands": {
    "git": ["status", "diff", "log"]
  }
}
```

**Example: Full development permissions:**
```json
{
  "commands": {
    "git": true,
    "npm": true,
    "node": true,
    "bun": true
  }
}
```

This is **more secure than containers** because:
1. Permission is checked at the command+argument level, not just syscall level
2. The WASM sandbox cannot execute arbitrary binaries not in the allowlist
3. The config is auditable and explicit (no hidden capabilities)

## Project Structure

```
edgebox/
├── build.zig              # Zig build configuration
├── vendor/
│   ├── quickjs-ng/        # QuickJS-NG C source (vendored)
│   └── wamr/              # WAMR runtime (submodule)
│       └── wamr-compiler/ # AOT compiler (requires LLVM)
└── src/
    ├── edgebox_wamr.zig   # WAMR runtime (recommended)
    ├── wasm_main.zig      # WASM entry point & polyfills
    ├── quickjs_core.zig   # QuickJS Zig bindings
    ├── wasm_fetch.zig     # HTTP/HTTPS fetch via WASI sockets
    ├── wasi_tls.zig       # TLS 1.3 client (X25519 + AES-GCM)
    ├── wasi_sock.zig      # WASI socket bindings
    └── wasi_process.zig   # Process spawning (wasmedge_process compatible API)
```

## Node.js Compatibility

All 58 compatibility tests pass. Run `edgebox run test/test_node_compat.js` to verify.

| API | Status | Notes |
|-----|--------|-------|
| `globalThis` | ✅ | |
| `console` | ✅ | log, error, warn, info |
| `process` | ✅ | env, argv, cwd, exit, platform, stdin, stdout, stderr |
| `Buffer` | ✅ | from, alloc, concat, toString |
| `fetch` | ✅ | HTTP and HTTPS (TLS 1.3) |
| `Promise` | ✅ | async/await |
| `setTimeout/setInterval` | ✅ | Polyfilled (synchronous execution) |
| `queueMicrotask` | ✅ | |
| `TextEncoder/TextDecoder` | ✅ | UTF-8 support |
| `URL/URLSearchParams` | ✅ | Full URL parsing |
| `AbortController/AbortSignal` | ✅ | Request cancellation |
| `crypto` | ✅ | randomUUID, getRandomValues |
| `require()` | ✅ | CommonJS module loader |
| `fs` module | ✅ | Sync operations + promises |
| `path` module | ✅ | join, resolve, parse, etc. |
| `events` module | ✅ | EventEmitter |
| `util` module | ✅ | format, promisify |
| `os` module | ✅ | platform, arch, homedir |
| `tty` module | ✅ | isatty, ReadStream, WriteStream |
| `child_process` | ✅ | spawnSync, execSync (wasmedge_process compatible API) |
| `stream` module | ✅ | Stub module |
| `http/https` modules | ✅ | Stub modules |
| `net` module | ✅ | Stub module |
| `dns` module | ✅ | Stub module |

## WASI Capabilities

| Capability | Status | Description |
|------------|--------|-------------|
| `filesystem` | ✅ | fd_read, fd_write, path_open |
| `environ` | ✅ | environ_get, environ_sizes_get |
| `args` | ✅ | args_get, args_sizes_get |
| `clock` | ✅ | clock_time_get |
| `random` | ✅ | random_get |
| `sockets` | ✅ | WAMR sock_open, sock_connect, sock_send, sock_recv |
| `tty` | ✅ | fd_fdstat_get for isatty detection |
| `process` | ✅ | wasmedge_process compatible API |

## Generated Files

| File | Description |
|------|-------------|
| `edgebox-base.wasm` | QuickJS + polyfills as WASM (1.6MB) |
| `edgebox.aot` | AOT-compiled native module (2.6MB) |
| `edgebox-wamr` | WAMR runtime binary (465KB) |

## Building from Source

```bash
# Build WAMR runtime (recommended)
zig build wamr -Doptimize=ReleaseFast

# Build WASM module
zig build wasm -Doptimize=ReleaseFast

# Build everything
zig build -Doptimize=ReleaseFast

# Run tests
zig build test
```

## License

Apache License 2.0

**Vendored dependencies:**
- QuickJS-NG: MIT License (see `vendor/quickjs-ng/LICENSE`)
- WAMR: Apache 2.0 License (see `vendor/wamr/LICENSE`)
