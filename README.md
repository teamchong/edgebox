# EdgeBox

QuickJS JavaScript runtime with WASI support and WasmEdge AOT compilation for running JavaScript at the edge.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                       EdgeBox                                │
├─────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐  │
│  │  QuickJS-NG │  │   WASI      │  │    WasmEdge AOT     │  │
│  │  (ES2023)   │──│  (preview1) │──│    Compiler         │  │
│  │  [vendored] │  │             │  │                     │  │
│  └─────────────┘  └─────────────┘  └─────────────────────┘  │
├─────────────────────────────────────────────────────────────┤
│  ┌─────────────────────────────────────────────────────────┐│
│  │              Node.js Polyfills                           ││
│  │  - Buffer, path, events, util, os, tty                  ││
│  │  - process.stdin/stdout/stderr, env, argv               ││
│  │  - fetch (HTTP), child_process (spawnSync)              ││
│  └─────────────────────────────────────────────────────────┘│
└─────────────────────────────────────────────────────────────┘
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

### Build

```bash
./build.sh                          # Build default (examples/hello)
./build.sh examples/claude-code     # Build Claude Code example
./build.sh my-app/                  # Build custom app directory
./build.sh --clean                  # Clean and rebuild
./build.sh --no-aot                 # Skip AOT compilation
```

### Run

```bash
./run.sh                            # Run the bundled app
./run.sh script.js                  # Run a JavaScript file
./run.sh -e "print('hello')"        # Evaluate JavaScript code
./run.sh -- --help                  # Pass args to the JS app
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
    ├── wasm_fetch.zig    # HTTP fetch via WASI sockets
    ├── wasi_sock.zig     # WasmEdge socket bindings
    ├── wasi_tty.zig      # TTY/terminal support
    └── wasi_process.zig  # Process spawning (WasmEdge plugin)
```

## Node.js Compatibility

### Implemented ✅

| API | Status | Notes |
|-----|--------|-------|
| `globalThis` | ✅ | |
| `console` | ✅ | log, error, warn, info |
| `process` | ✅ | env, argv, cwd, exit, platform, stdin, stdout, stderr |
| `Buffer` | ✅ | from, alloc, concat, toString |
| `fetch` | ✅ | HTTP only (HTTPS not yet supported) |
| `Promise` | ✅ | async/await |
| `queueMicrotask` | ✅ | |
| `path` module | ✅ | join, resolve, parse, etc. |
| `events` module | ✅ | EventEmitter |
| `util` module | ✅ | format, promisify |
| `os` module | ✅ | platform, arch, homedir |
| `tty` module | ✅ | isatty, ReadStream, WriteStream |
| `child_process` | ✅ | spawnSync, execSync (requires WasmEdge process plugin) |

### Not Yet Implemented ❌

| API | Priority | Notes |
|-----|----------|-------|
| `setTimeout/setInterval` | 🔴 High | Timer functions |
| `TextEncoder/TextDecoder` | 🔴 High | Text encoding |
| `URL/URLSearchParams` | 🔴 High | URL parsing |
| `AbortController/AbortSignal` | 🔴 High | Request cancellation |
| `crypto` | 🔴 High | randomUUID, getRandomValues |
| `require()` | 🔴 High | CommonJS module loader |
| `fs` module | 🔴 High | File system operations |
| `http/https` modules | 🟡 Medium | HTTP server/client |
| `stream` module | 🟡 Medium | Readable/Writable streams |
| `net` module | 🟡 Medium | TCP sockets |
| `dns` module | 🟢 Low | DNS resolution |

Run `./run.sh test/test_node_compat.js` to see current compatibility status.

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
| `bundle.js` | Bundled app (minified) |
| `.edgebox.json` | App config (copied from app directory) |
| `edgebox-base.wasm` | QuickJS WASM module |
| `edgebox-aot.dylib` | AOT-compiled native module (macOS) |
| `edgebox-aot.so` | AOT-compiled native module (Linux) |

## License

Apache License 2.0

**Vendored dependencies:**
- QuickJS-NG: MIT License (see `vendor/quickjs-ng/LICENSE`)
