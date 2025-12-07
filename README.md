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

1. **WasmEdge** (required):
```bash
# macOS
brew install wasmedge

# Linux
curl -sSf https://raw.githubusercontent.com/WasmEdge/WasmEdge/master/utils/install.sh | bash
```

2. **Node.js 18+** (for bundling Claude Code):
```bash
node --version  # Should be v18+
```

3. **esbuild** (for JS bundling):
```bash
npm install -g esbuild
```

### Build

```bash
./build.sh              # Full build: bundle Claude Code + compile WASM + AOT
./build.sh --no-aot     # Skip AOT compilation
./build.sh --clean      # Clean and rebuild
```

### Run

```bash
./run.sh                           # Interactive mode
./run.sh script.js                 # Run a JavaScript file
./run.sh -e "console.log('hi')"    # Evaluate JavaScript code
./run.sh --aot script.js           # Run with AOT (faster startup)
./run.sh --claude "Fix the bug"    # Run Claude Code
```

## What It Does

### build.sh

1. **Installs Claude Code**: Downloads `@anthropic-ai/claude-code` from npm
2. **Bundles JS**: Uses esbuild to bundle into a single `bundle.js`
3. **Generates WASM**: Creates QuickJS WASM module with WASI support
4. **AOT Compiles**: Uses WasmEdge to compile WASM to native code

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
packages/edgebox/
├── build.sh           # Build script (bundle + WASM + AOT)
├── run.sh             # Run script (WasmEdge executor)
├── package.json       # Package manifest
├── build.zig          # Zig build configuration
├── vendor/
│   └── quickjs-ng/    # QuickJS-NG C source (committed)
└── src/
    ├── main.zig       # Main entry point & Runtime API
    ├── quickjs.zig    # QuickJS Zig bindings
    ├── wasi.zig       # WASI syscall layer
    ├── aot.zig        # AOT compilation support
    ├── node_compat.zig # Node.js API compatibility
    └── cli.zig        # CLI entry point
```

## WASI Capabilities

| Capability | Status | Description |
|------------|--------|-------------|
| `filesystem` | ✅ | fd_read, fd_write, path_open |
| `environ` | ✅ | environ_get, environ_sizes_get |
| `args` | ✅ | args_get, args_sizes_get |
| `clock` | ✅ | clock_time_get |
| `random` | ✅ | random_get |
| `sockets` | 🚧 | sock_accept, sock_recv (preview2) |

## Node.js Compatibility

EdgeBox provides Node.js-compatible APIs for Claude Code:

- `fs` / `fs/promises` - File system operations
- `path` - Path manipulation
- `Buffer` - Binary data handling
- `crypto` - Random, hashing
- `http` / `https` - HTTP client (via fetch)
- `events` - EventEmitter
- `stream` - Stream APIs
- `util`, `url` - Utilities

## Environment Variables

| Variable | Description |
|----------|-------------|
| `ANTHROPIC_API_KEY` | Required for Claude Code |
| `WASMEDGE_DIR` | WasmEdge installation path |
| `HOME` | Passed to WASI for config files |

## Building QuickJS WASM (Advanced)

To build a full QuickJS WASM module with all features:

```bash
# Install wasi-sdk
# macOS: brew install --cask aspect/packages/wasi-sdk
# Linux: Download from https://github.com/nickg/nickg-WebAssembly/wasi-sdk

cd vendor/quickjs-ng
make CROSS_PREFIX=/opt/wasi-sdk/bin/ qjs.wasm
cp qjs.wasm ../../edgebox-base.wasm
```

## License

Apache License 2.0 - See [LICENSE](../../LICENSE) in the repository root.

**Vendored dependencies:**
- QuickJS-NG: MIT License (see `vendor/quickjs-ng/LICENSE`)
