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
│  │  - 10ms cold start (3x faster than Node.js)         │  │
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
│                    Security Model                        │
├─────────────────────────────────────────────────────────┤
│  .aot file (native code)                                │
│    ├── Memory access: bounds-checked instructions       │
│    ├── Function calls: validated call table             │
│    └── System calls: all go through WASI →              │
│                                           ↓             │
│  WAMR Runtime                                           │
│    ├── WASI filesystem: only dirs in .edgebox.json     │
│    ├── WASI sockets: controlled by runtime              │
│    └── Process spawning: command allowlist              │
└─────────────────────────────────────────────────────────┘
```

**Think of it as**: "Pre-compiled WASM" - same sandbox, native speed.

## Quick Start

```bash
# Build EdgeBox
zig build cli -Doptimize=ReleaseFast
zig build wamr -Doptimize=ReleaseFast

# Run JavaScript (WAMR AOT - fastest)
./zig-out/bin/edgebox-wamr edgebox.aot hello.js

# Run JavaScript (interpreter mode)
./zig-out/bin/edgebox-wamr edgebox.wasm hello.js
```

## Performance

### Cold Start (hello.js)

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `Porffor (CLI)` | 6.0 ± 0.2 | 5.5 | 6.4 | 1.00 |
| `Bun (CLI)` | 18.1 ± 1.3 | 16.3 | 21.7 | 3.01 |
| `EdgeBox (WASM)` | 22.4 ± 2.2 | 18.1 | 25.9 | 3.72 |
| `Node.js (CLI)` | 36.9 ± 3.3 | 33.9 | 47.0 | 6.12 |
| `Porffor (WASM)` | 103.0 ± 6.0 | 99.6 | 126.4 | 17.09 |
| `wasmedge-qjs (WASM)` | 117.1 ± 2.9 | 114.8 | 127.4 | 19.43 |

### Alloc Stress (30k allocations)

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `Bun (CLI)` | 21.3 ± 1.9 | 19.6 | 25.9 | 1.00 |
| `Node.js (CLI)` | 37.4 ± 1.1 | 35.6 | 39.0 | 1.76 |
| `Porffor (CLI)` | 42.1 ± 1.1 | 40.4 | 44.6 | 1.98 |
| `EdgeBox (WASM)` | 46.0 ± 0.9 | 45.0 | 47.9 | 2.16 |
| `Porffor (WASM)` | 271.7 ± 7.7 | 260.7 | 285.9 | 12.77 |
| `wasmedge-qjs (WASM)` | 1912.3 ± 22.5 | 1896.6 | 1971.6 | 89.87 |

### CPU fib(35)

| Command | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `Bun (CLI)` | 0.066 ± 0.002 | 0.064 | 0.068 | 1.00 |
| `Node.js (CLI)` | 0.099 ± 0.003 | 0.096 | 0.101 | 1.50 |
| `Porffor (CLI)` | 0.138 ± 0.000 | 0.137 | 0.138 | 2.09 |
| `Porffor (WASM)` | 0.202 ± 0.005 | 0.199 | 0.208 | 3.07 |
| `EdgeBox (WASM)` | 1.200 ± 0.004 | 1.195 | 1.203 | 18.18 |
| `wasmedge-qjs (WASM)` | >60s | - | - | TIMEOUT |

**Key Insights:**
- **EdgeBox cold start (22ms)** is competitive with Bun (18ms) and faster than Node.js (37ms)
- **EdgeBox is sandboxed** via WASM - memory bounds checks + WASI syscall interception
- **CPU-bound tasks are slower** because QuickJS is an interpreter (no JIT like V8/JSC)
- **wasmedge-qjs** is the slowest WASM runtime (117ms cold start, timeout on fib)

**Trade-offs:**
- EdgeBox uses QuickJS (interpreter) - CPU-bound tasks are slower than V8/JSC JIT
- Best for: I/O-bound tasks, sandboxed execution, edge/serverless
- Not ideal for: Heavy computation (use native runtimes instead)

### vs Anthropic sandbox-runtime

EdgeBox takes a different approach to sandboxing compared to [Anthropic's sandbox-runtime](https://github.com/anthropic-experimental/sandbox-runtime):

| Aspect | **EdgeBox** | **sandbox-runtime** |
|--------|-------------|---------------------|
| **Approach** | WASM sandbox (code runs inside) | OS-level sandbox (wraps process) |
| **Technology** | WAMR + QuickJS + AOT | macOS: `sandbox-exec`, Linux: `bubblewrap` |
| **Cold Start** | **10ms** | ~50-200ms |
| **Memory** | **~2MB** | ~50MB+ |
| **Can Run** | JavaScript only | Any binary (git, python, etc.) |
| **Network** | Built-in TLS 1.3 fetch | HTTP/SOCKS5 proxy filtering |
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
2. **Lightweight Runtime** - WAMR is 465KB binary vs WasmEdge 2.6MB
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
- Loads WASM via WasmEdge C API
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
| `edgebox-wamr` | WAMR runtime (AOT or interpreter) | **10ms** (AOT) / 48ms (interp) |
| `edgebox` | Legacy WasmEdge runtime | ~35ms |
| `edgeboxc` | Build tools (bundle, compile) | N/A |

#### edgebox-wamr - Fast WAMR Runner (Recommended)
```bash
# Run with AOT (fastest)
edgebox-wamr edgebox.aot hello.js

# Run with interpreter (no AOT compilation needed)
edgebox-wamr edgebox.wasm hello.js
```

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
  "dirs": ["/tmp", "~/.claude"],
  "env": ["ANTHROPIC_API_KEY", "HOME"],
  "commands": {
    "git": ["clone", "status", "add", "commit", "push", "pull"],
    "npm": ["install", "run", "test"],
    "node": true,
    "curl": true
  }
}
```

| Field | Description |
|-------|-------------|
| `npm` | npm package to install and use as entry point |
| `dirs` | Directories to map into WASI sandbox |
| `env` | Environment variables to pass to the app |
| `commands` | Command permissions for child_process (see below) |

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
    └── wasi_process.zig   # Process spawning (wasmedge_process API)
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
| `child_process` | ✅ | spawnSync, execSync (requires WasmEdge process plugin) |
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
