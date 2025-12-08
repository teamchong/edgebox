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
| **Cold Start** | **14.4ms** | 14.6ms | 17.6ms | 32.2ms | 100ms |
| **Alloc Stress** | **13.7ms** | 19.5ms | 34.5ms | 35.1ms | - |
| **CPU fib(35)** | 1,310ms | **56ms** | 1,500ms | 94ms | 182ms |

**Key Results:**
- **Cold Start**: EdgeBox is **fastest** - beats Bun by 1%, wasmedge-quickjs by 22%
- **Alloc Stress**: EdgeBox is **42% faster** than Bun, **2.5x faster** than wasmedge-quickjs
- **CPU**: Native runtimes (Bun/Node) have JIT advantage; EdgeBox is 13% faster than wasmedge-quickjs

### Optimizations

1. **Bump Allocator** - O(1) malloc (pointer bump), NO-OP free (memory reclaimed at exit)
2. **wasm-opt -Oz** - 82% binary size reduction (5.8MB → 1.1MB WASM)
3. **Lazy Polyfills** - Only inject minimal bootstrap on startup, load Node.js polyfills on-demand
4. **Bytecode Caching** - Pre-compile JavaScript at build time, skip parsing at runtime
5. **AOT Compilation** - WasmEdge compiles WASM to native code

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
