# EdgeBox

QuickJS JavaScript runtime with WASI support and WasmEdge AOT compilation for running Claude Code at the edge.

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
│  │              Claude Code Runtime                         ││
│  │  - Node.js compatible APIs (fs, path, http, crypto)     ││
│  │  - npm package bundling via esbuild                     ││
│  │  - WASM sandbox execution                               ││
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

3. **Zig** (optional, for building WASM from source):
```bash
# macOS
brew install zig

# Linux: https://ziglang.org/download/
```

### Build

```bash
./build.sh              # Full build: bundle Claude Code + compile WASM + AOT
./build.sh --no-aot     # Skip AOT compilation
./build.sh --clean      # Clean and rebuild
```

### Run

```bash
./run.sh                                    # Run default example (hello)
./run.sh examples/claude-code/index.js      # Run Claude Code example
./run.sh script.js                          # Run a JavaScript file
./run.sh -e "print('hello')"                # Evaluate JavaScript code
./run.sh -- -p "prompt"                     # Pass args to JS app
```

## What It Does

### build.sh

1. **Bundles JS**: Uses Bun to bundle your app into a single `bundle.js`
2. **Compiles WASM**: Uses Zig to compile QuickJS to WASM with WASI support
3. **AOT Compiles**: Uses WasmEdge to compile WASM to native code (6x faster)

### run.sh

1. **Sets up WASI**: Maps directories and environment variables
2. **Runs WasmEdge**: Executes the WASM/AOT module
3. **Handles modes**: Interactive, script file, eval, or Claude Code

## Generated Files

| File | Description |
|------|-------------|
| `bundle.js` | Bundled Claude Code (minified) |
| `edgebox-base.wasm` | QuickJS WASM module |
| `edgebox.aot` | AOT-compiled native module |
| `node_modules/` | npm dependencies (gitignored) |

## Vendored Dependencies

QuickJS-NG is vendored in `vendor/quickjs-ng/` and committed to git. No external download needed.

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
│       └── index.js
├── test/
│   └── test_features.js  # Feature test suite (38 tests)
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

## Node.js Compatibility

EdgeBox provides Node.js-compatible APIs:

| Module | Status | Description |
|--------|--------|-------------|
| `Buffer` | ✅ | Binary data with UTF-8 encoding/decoding |
| `path` | ✅ | Full path manipulation (join, resolve, parse, etc.) |
| `url` | ✅ | URL parsing (wraps built-in URL class) |
| `events` | ✅ | EventEmitter (on, once, off, emit) |
| `util` | ✅ | format, promisify, callbackify, types |
| `os` | ✅ | platform, arch, homedir, tmpdir |
| `tty` | ✅ | isatty, ReadStream, WriteStream |
| `child_process` | ✅ | spawnSync, execSync (requires WasmEdge process plugin) |
| `process` | ✅ | stdin, stdout, stderr, env, argv, platform |
| `fetch` | ✅ | HTTP client (HTTPS not yet supported) |

## Environment Variables

| Variable | Description |
|----------|-------------|
| `ANTHROPIC_API_KEY` | Required for Claude Code |
| `WASMEDGE_DIR` | WasmEdge installation path |
| `HOME` | Passed to WASI for config files |

## Building QuickJS WASM (Advanced)

EdgeBox uses Zig to compile QuickJS to WASM (no wasi-sdk required):

```bash
# Install Zig (if not already installed)
# macOS
brew install zig

# Linux (check https://ziglang.org/download/ for latest)
wget https://ziglang.org/download/0.15.2/zig-linux-x86_64-0.15.2.tar.xz
tar xf zig-linux-x86_64-0.15.2.tar.xz
export PATH=$PWD/zig-linux-x86_64-0.15.2:$PATH

# Build WASM
zig build -Dtarget=wasm32-wasi -Doptimize=ReleaseSmall

# Or use the build script
./build.sh
```

## License

Apache License 2.0 - See [LICENSE](../../LICENSE) in the repository root.

**Vendored dependencies:**
- QuickJS-NG: MIT License (see `vendor/quickjs-ng/LICENSE`)
