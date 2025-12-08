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
│  │  - QuickJS runtime/context pre-built at compile    │  │
│  │  - Static polyfills snapshotted into WASM binary   │  │
│  │  - 0.03ms cold start (350x faster than cold boot)  │  │
│  └─────────────────────────────────────────────────────┘  │
└───────────────────────────────────────────────────────────┘
```

## Quick Start

### Prerequisites

1. **Bun** (required for bundling):
```bash
# macOS
brew install oven-sh/bun/bun

# Linux/macOS (alternative)
curl -fsSL https://bun.sh/install | bash
```

2. **WasmEdge** (required for runtime):
```bash
# macOS
brew install wasmedge

# Linux
curl -sSf https://raw.githubusercontent.com/WasmEdge/WasmEdge/master/utils/install.sh | bash
```

3. **Zig** (required for building WASM):
```bash
# macOS
brew install zig

# Linux: https://ziglang.org/download/
```

### Build & Run

```bash
./run.sh                            # Build and run default (examples/hello)
./run.sh examples/claude-code       # Build and run Claude Code example
./run.sh script.js                  # Run a JavaScript file
./run.sh -e "print('hello')"        # Evaluate JavaScript code
./run.sh -- --help                  # Pass args to the JS app
```

Build only (without running):
```bash
./build.sh                          # Build default (examples/hello)
./build.sh examples/claude-code     # Build Claude Code example
./build.sh --clean                  # Clean and rebuild
./build.sh --no-aot                 # Skip AOT compilation
```

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
├── build.sh           # Build script (bundle + WASM + AOT)
├── run.sh             # Run script (WasmEdge executor)
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

All 58 compatibility tests pass. Run `./run.sh test/test_node_compat.js` to verify.

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

## Performance

### Benchmarks

All WASM runtimes use WasmEdge with AOT compilation. Run `./bench/run_hyperfine.sh` to reproduce.

| Test | EdgeBox | Bun | wasmedge-qjs | Node.js | Porffor |
|------|---------|-----|--------------|---------|---------|
| **Cold Start** | 20ms | **14ms** | 17ms | 31ms | 99ms |
| **Alloc Stress** | 45ms | **17ms** | 36ms | 36ms | - |
| **CPU fib(35)×100** | 122s | **4.3s** | 150s | 6.3s | 7.5s |

**Internal Cold Start** (JS engine init, measured inside WASM):
- With Wizer pre-init: **0.3ms**
- Without Wizer: ~10ms

**Key Results:**
- **Wizer Pre-init**: 30x faster JS engine initialization (0.3ms vs 10ms)
- **CPU Performance**: EdgeBox is 19% faster than wasmedge-quickjs for CPU-bound work
- **Sandboxed Execution**: Full WASI isolation with HTTPS/TLS support

### Optimizations

1. **Wizer Pre-initialization** - QuickJS runtime/context pre-built at compile time (0.03ms startup)
2. **WASM SIMD128** - 16-byte vector operations for string processing
3. **Bump Allocator** - O(1) malloc (pointer bump), NO-OP free (memory reclaimed at exit)
4. **wasm-opt -Oz** - 82% binary size reduction (5.8MB → 1.1MB WASM)
5. **Lazy Polyfills** - Only inject minimal bootstrap on startup, load Node.js polyfills on-demand
6. **Bytecode Caching** - Pre-compile JavaScript at build time, skip parsing at runtime
7. **AOT Compilation** - WasmEdge compiles WASM to native code

### Build-time Bytecode Caching

EdgeBox pre-compiles JavaScript to QuickJS bytecode at build time for fast startup:

```
./build.sh my-app/
  ↓
bundle.js (12KB)     → JavaScript source + polyfills
bundle.js.cache (29KB) → Pre-compiled bytecode

./run.sh
  ↓
Load bytecode → Execute (skips JS parsing)
```

The bytecode cache is automatically invalidated when polyfills change (via hash check).

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

## Generated Files

| File | Description |
|------|-------------|
| `bundle.js` | Bundled app with polyfills (minified) |
| `bundle.js.cache` | Pre-compiled QuickJS bytecode |
| `.edgebox.json` | App config (copied from app directory) |
| `edgebox-base.wasm` | QuickJS WASM module |
| `edgebox-aot.dylib` | AOT-compiled native module (macOS) |
| `edgebox-aot.so` | AOT-compiled native module (Linux) |

## License

Apache License 2.0

**Vendored dependencies:**
- QuickJS-NG: MIT License (see `vendor/quickjs-ng/LICENSE`)
