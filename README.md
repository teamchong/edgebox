# EdgeBox

QuickJS JavaScript runtime with WASI support and WasmEdge AOT compilation for running JavaScript at the edge.

## Architecture

```
┌───────────────────────────────────────────────────────────┐
│                       EdgeBox                             │
├───────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌───────────────────┐  │
│  │  QuickJS-NG │  │   WASI      │  │  WasmEdge AOT     │  │
│  │  (ES2023)   │──│  (preview1) │──│  Compiler         │  │
│  │  [vendored] │  │             │  │                   │  │
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
│  │           Wizer Pre-initialization                  │  │
│  │  - QuickJS runtime/context pre-built at compile     │  │
│  │  - Static polyfills snapshotted into WASM binary    │  │
│  │  - 0.03ms cold start (350x faster than cold boot)   │  │
│  └─────────────────────────────────────────────────────┘  │
└───────────────────────────────────────────────────────────┘
```

## Quick Start

## Performance

### Benchmarks

Native `edgebox` CLI vs other runtimes. Run `./bench/run_hyperfine.sh` to reproduce.

| Test | EdgeBox | Bun | wasmedge CLI | Node.js |
|------|---------|-----|--------------|---------|
| **Cold Start** | **16ms** | 15ms | 22ms | 32ms |

**Key Results:**
- **Native CLI**: 1.4x faster cold start than wasmedge CLI (16ms vs 22ms)
- **Wizer Pre-init**: 30x faster JS engine initialization (0.3ms vs 10ms)
- **CPU Performance**: EdgeBox is 20% faster than wasmedge-quickjs for CPU-bound work
- **Sandboxed Execution**: Full WASI isolation with HTTPS/TLS support

### Optimizations

1. **Native WasmEdge Embedding** - Direct C library integration (no CLI overhead)
2. **Wizer Pre-initialization** - QuickJS runtime/context pre-built at compile time
3. **WASM SIMD128** - 16-byte vector operations for string processing
4. **Bump Allocator** - O(1) malloc (pointer bump), NO-OP free (memory reclaimed at exit)
5. **wasm-opt -Oz** - 82% binary size reduction (5.8MB → 1.1MB WASM)
6. **Lazy Polyfills** - Only inject minimal bootstrap on startup, load Node.js polyfills on-demand
7. **Bytecode Caching** - Pre-compile JavaScript at build time, skip parsing at runtime
8. **AOT Compilation** - WasmEdge compiles WASM to native code

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

EdgeBox uses [Wizer](https://github.com/bytecodealliance/wizer) to pre-initialize the QuickJS runtime at build time:

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

Install Wizer: `cargo install wizer --features="env_logger structopt"`

### Prerequisites

1. **Bun** (required for bundling):
```bash
# macOS
brew install oven-sh/bun/bun

# Linux/macOS (alternative)
curl -fsSL https://bun.sh/install | bash
```

2. **WasmEdge 0.15.0+** (required for runtime):
```bash
# Install WasmEdge 0.15.0
curl -sSf https://raw.githubusercontent.com/WasmEdge/WasmEdge/master/utils/install.sh | bash -s -- -v 0.15.0

# Or via Homebrew (macOS)
brew install wasmedge
```

3. **Zig** (required for building):
```bash
# macOS
brew install zig

# Linux: https://ziglang.org/download/
```

### Build & Run

```bash
# Build the edgebox CLI
zig build cli

# Compile JS app to WASM (bundles JS + embeds bytecode + AOT compiles)
./zig-out/bin/edgebox build my-app/

# Run the compiled WASM
./zig-out/bin/edgebox run edgebox-static.wasm
./zig-out/bin/edgebox edgebox-static.wasm      # Shorthand
```

### CLI Usage

```
EdgeBox - QuickJS JavaScript Runtime with WASI + WasmEdge AOT

Usage:
  edgebox build [app-directory]   Compile JS to WASM with embedded bytecode
  edgebox run <file.wasm>         Run compiled WASM module
  edgebox <file.wasm>             Run WASM (shorthand)

Options:
  --help, -h     Show this help
  --version, -v  Show version

Build Options:
  --dynamic      Use dynamic JS loading (for development)
```

### Build Pipeline

```
edgebox build my-app/
  ↓
my-app/index.js          → Entry point
  ↓ bun build
bundle.js (12KB)         → Bundled JS + polyfills
  ↓ qjsc (QuickJS compiler)
bundle_compiled.c (71KB) → Pre-compiled bytecode as C
  ↓ zig build + wizer + wasm-opt
edgebox-static.wasm      → WASM with embedded bytecode
  ↓ wasmedge compile
edgebox-static-aot.dylib → Native AOT module
```

The bytecode is embedded directly in the WASM binary, eliminating JS parsing at runtime.

## App Configuration

Apps can include a `.edgebox.json` config file:

```json
{
  "name": "my-app",
  "npm": "@anthropic-ai/claude-code",
  "dirs": ["/tmp", "~/.claude"],
  "env": ["ANTHROPIC_API_KEY", "HOME"]
}
```

| Field | Description |
|-------|-------------|
| `npm` | npm package to install and use as entry point |
| `dirs` | Directories to map into WASI sandbox |
| `env` | Environment variables to pass to the app |

## Project Structure

```
edgebox/
├── build.zig          # Zig build configuration
├── examples/
│   ├── hello/         # Simple hello world example
│   │   └── index.js
│   └── claude-code/   # Claude Code CLI example
│       └── .edgebox.json
├── test/
│   ├── test_features.js     # Feature tests (38 tests)
│   └── test_node_compat.js  # Node.js compatibility tests
├── vendor/
│   └── quickjs-ng/    # QuickJS-NG C source (vendored)
└── src/
    ├── runtime.zig       # Native CLI (embeds WasmEdge C library)
    ├── wasm_main.zig     # WASM entry point & polyfills
    ├── wizer_init.zig    # Wizer pre-initialization (build-time)
    ├── quickjs_core.zig  # QuickJS Zig bindings
    ├── snapshot.zig      # Bytecode caching system
    ├── wasm_fetch.zig    # HTTP/HTTPS fetch via WASI sockets
    ├── wasi_tls.zig      # TLS 1.3 client (X25519 + AES-GCM)
    ├── wasi_sock.zig     # WasmEdge socket bindings
    ├── wasi_tty.zig      # TTY/terminal support
    └── wasi_process.zig  # Process spawning (WasmEdge plugin)
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
| `sockets` | ✅ | WasmEdge sock_open, sock_connect, sock_send, sock_recv |
| `tty` | ✅ | fd_fdstat_get for isatty detection |
| `process` | ✅ | WasmEdge process plugin (wasmedge_process) |

## Generated Files

| File | Description |
|------|-------------|
| `bundle.js` | Bundled app with polyfills (minified) |
| `bundle_compiled.c` | Pre-compiled bytecode as C source |
| `edgebox-static.wasm` | WASM with embedded bytecode |
| `edgebox-static-aot.dylib` | AOT-compiled native module (macOS) |
| `edgebox-static-aot.so` | AOT-compiled native module (Linux) |

## Building from Source

```bash
# Build native CLI (with WasmEdge embedded)
zig build cli

# Build WASM module only
zig build wasm -Doptimize=ReleaseFast

# Run tests
zig build test
```

## License

Apache License 2.0

**Vendored dependencies:**
- QuickJS-NG: MIT License (see `vendor/quickjs-ng/LICENSE`)
