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

# Run JavaScript with pre-compiled WASM (includes QuickJS)
./zig-out/bin/edgebox hello.wasm

# Run JavaScript file directly (uses edgebox-base.wasm)
./zig-out/bin/edgebox hello.js
```

## Performance

Benchmarks run on WAMR (WebAssembly Micro Runtime) with **AOT compilation** for maximum performance. Tests 6 runtimes across 4 benchmarks.

### Cold Start (hello.js)

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `Porffor (CLI)` | 6.8 ± 0.4 | 6.4 | 8.2 | 1.00 |
| `EdgeBox (AOT)` | 13.2 ± 0.4 | 12.6 | 13.9 | 1.93 |
| `EdgeBox (daemon)` | 16.2 ± 0.3 | 15.7 | 17.1 | 2.38 |
| `Bun (CLI)` | 17.7 ± 0.5 | 16.6 | 18.5 | 2.59 |
| `Node.js (CLI)` | 35.5 ± 0.9 | 34.0 | 38.4 | 5.21 |
| `Porffor (WASM)` | 98.7 ± 0.9 | 97.4 | 101.1 | 14.47 |

### Alloc Stress (30k allocations)

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `Bun (CLI)` | 20.2 ± 0.4 | 19.7 | 21.0 | 1.00 |
| `EdgeBox (AOT)` | 25.3 ± 0.4 | 24.8 | 26.1 | 1.25 |
| `EdgeBox (daemon)` | 26.9 ± 2.3 | 20.3 | 28.1 | 1.33 |
| `Node.js (CLI)` | 38.5 ± 1.1 | 37.3 | 40.9 | 1.90 |
| `Porffor (CLI)` | 51.0 ± 1.2 | 49.6 | 53.0 | 2.52 |
| `Porffor (WASM)` | 283.9 ± 7.4 | 271.3 | 295.8 | 14.03 |

### CPU fib(35)

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `Bun (CLI)` | 61.0 ± 0.2 | 60.8 | 61.1 | 1.00 |
| `Node.js (CLI)` | 98.2 ± 0.7 | 97.4 | 98.7 | 1.61 |
| `Porffor (CLI)` | 138.4 ± 0.5 | 138.1 | 139.0 | 2.27 |
| `Porffor (WASM)` | 198.2 ± 3.5 | 194.2 | 200.4 | 3.25 |
| `EdgeBox (AOT)` | 987.4 ± 3.9 | 984.0 | 991.6 | 16.19 |
| `EdgeBox (daemon)` | 986.5 ± 3.6 | 984.3 | 990.6 | 16.17 |

### Daemon Warm Pod (pre-allocated batch pool)

| Command | Mean [ms] | Min [ms] | Max [ms] | Notes |
|:---|---:|---:|---:|:---|
| `EdgeBox (daemon warm)` | 9.7 ± 0.8 | 8.3 | 11.4 | curl + HTTP overhead |
| Server-side execution | ~1-2 | - | - | WASM exec only |

**Key Insights:**
- **EdgeBox cold start (13ms)** is faster than Bun (18ms) and Node.js (36ms)
- **Daemon warm pod (~10ms)** includes curl + HTTP overhead; actual WASM execution is **~1-2ms**
- **EdgeBox is sandboxed** via WASM - memory bounds checks + WASI syscall interception
- **CPU-bound tasks are slower** because QuickJS is an interpreter (no JIT like V8/JSC)
- Binary size: **454KB** (vs 2.7MB with WasmEdge)

**Trade-offs:**
- EdgeBox uses QuickJS (interpreter) - CPU-bound tasks are ~16x slower than V8 JIT
- Best for: Sandboxed execution, I/O-bound tasks, edge deployment
- Not ideal for: Heavy computation (use native runtimes instead)

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
│  │  ┌────┐ ┌────┐ ┌────┐ ┌────┐ ┌────┐ ┌────┐ ┌────┐      │    │
│  │  │Inst│ │Inst│ │Inst│ │Inst│ │Inst│ │Inst│ │Inst│ ... │    │
│  │  │ 1  │ │ 2  │ │ 3  │ │ 4  │ │ 5  │ │ 6  │ │ 7  │      │    │
│  │  └────┘ └────┘ └────┘ └────┘ └────┘ └────┘ └────┘      │    │
│  │    ↑                                                    │    │
│  │   grab                                                  │    │
│  └─────────────────────────────────────────────────────────┘    │
│            ↑                                                    │
│  ┌─────────┴─────────┐                                          │
│  │  HTTP Server      │  Grabs ready instance (0ms!)            │
│  │  (main thread)    │  Executes → Destroys → Returns          │
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
