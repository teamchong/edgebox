#!/bin/bash
# EdgeBox Build Script
# Downloads Claude Code, sets up QuickJS WASM runtime, and AOT compiles
#
# Usage:
#   ./build.sh              # Full build
#   ./build.sh --no-aot     # Skip AOT compilation
#   ./build.sh --clean      # Clean build artifacts

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log() { echo -e "${GREEN}[build]${NC} $1"; }
warn() { echo -e "${YELLOW}[warn]${NC} $1"; }
error() { echo -e "${RED}[error]${NC} $1"; exit 1; }

# Parse arguments
NO_AOT=false
CLEAN=false
for arg in "$@"; do
    case $arg in
        --no-aot) NO_AOT=true ;;
        --clean) CLEAN=true ;;
        --help|-h)
            echo "EdgeBox Build Script"
            echo ""
            echo "Usage: ./build.sh [options]"
            echo ""
            echo "Options:"
            echo "  --no-aot    Skip AOT compilation (just build WASM)"
            echo "  --clean     Clean build artifacts before building"
            echo "  --help      Show this help"
            exit 0
            ;;
    esac
done

# Clean if requested
if [ "$CLEAN" = true ]; then
    log "Cleaning build artifacts..."
    rm -rf node_modules dist build *.wasm *.aot bundle.js .zig-cache zig-out
fi

# Step 1: Check prerequisites
log "Checking prerequisites..."

# Prefer bun if available, fallback to npm
PKG_MANAGER=""
PKG_INSTALL=""
if command -v bun &> /dev/null; then
    PKG_MANAGER="bun"
    PKG_INSTALL="bun install"
    log "Using Bun: $(bun --version)"
elif command -v node &> /dev/null && command -v npm &> /dev/null; then
    NODE_VERSION=$(node -v | cut -d'v' -f2 | cut -d'.' -f1)
    if [ "$NODE_VERSION" -lt 18 ]; then
        error "Node.js 18+ is required. Current: $(node -v)"
    fi
    PKG_MANAGER="npm"
    PKG_INSTALL="npm install"
    log "Using Node.js $(node -v), npm $(npm -v)"
else
    error "Either Bun or Node.js 18+ is required.
  Install Bun: curl -fsSL https://bun.sh/install | bash
  Or Node.js: https://nodejs.org"
fi

if ! command -v zig &> /dev/null; then
    warn "Zig not found - skipping Zig build step"
    HAS_ZIG=false
else
    HAS_ZIG=true
    log "Zig $(zig version)"
fi

# Step 2: Install Claude Code
log "Installing @anthropic-ai/claude-code..."
if [ ! -d "node_modules/@anthropic-ai/claude-code" ]; then
    $PKG_INSTALL
else
    log "Claude Code already installed, skipping..."
fi

# Verify installation
if [ ! -f "node_modules/@anthropic-ai/claude-code/cli.js" ]; then
    error "Claude Code cli.js not found after install"
fi

CLI_SIZE=$(wc -c < "node_modules/@anthropic-ai/claude-code/cli.js" | tr -d ' ')
log "Claude Code CLI: $(echo "scale=2; $CLI_SIZE/1024/1024" | bc)MB"

# Step 3: Copy Claude Code CLI as the bundle
# Claude Code is already pre-bundled as cli.js, no esbuild needed
log "Setting up Claude Code bundle..."
cp node_modules/@anthropic-ai/claude-code/cli.js bundle.js

# Copy tree-sitter WASM files (needed by Claude Code)
if [ -f "node_modules/@anthropic-ai/claude-code/tree-sitter.wasm" ]; then
    cp node_modules/@anthropic-ai/claude-code/tree-sitter.wasm .
    cp node_modules/@anthropic-ai/claude-code/tree-sitter-bash.wasm .
    log "Copied tree-sitter WASM files"
fi

# Step 4: Build Zig components (optional)
if [ "$HAS_ZIG" = true ]; then
    log "Building Zig components..."
    zig build -Doptimize=ReleaseSmall 2>&1 || {
        warn "Zig build failed (QuickJS C compilation may need adjustments)"
        warn "Continuing without Zig components..."
    }
fi

# Step 5: Generate WASM module for QuickJS runtime
log "Generating WASM module..."

# Check if we have wasmedge tools
HAS_WASMEDGE=false
WASMEDGEC=""
if command -v wasmedgec &> /dev/null; then
    WASMEDGEC="wasmedgec"
    HAS_WASMEDGE=true
elif [ -f "$HOME/.wasmedge/bin/wasmedgec" ]; then
    WASMEDGEC="$HOME/.wasmedge/bin/wasmedgec"
    HAS_WASMEDGE=true
fi

# Check if we have a pre-built quickjs wasm
if [ -f "vendor/quickjs.wasm" ]; then
    log "Using pre-built QuickJS WASM..."
    cp vendor/quickjs.wasm edgebox-base.wasm
elif [ -f "vendor/qjs.wasm" ]; then
    log "Using pre-built qjs WASM..."
    cp vendor/qjs.wasm edgebox-base.wasm
else
    # Try to build QuickJS WASM if wasi-sdk is available
    if [ -d "/opt/wasi-sdk" ] || [ -d "$HOME/wasi-sdk" ]; then
        WASI_SDK="${WASI_SDK:-/opt/wasi-sdk}"
        [ -d "$HOME/wasi-sdk" ] && WASI_SDK="$HOME/wasi-sdk"

        log "Building QuickJS WASM with wasi-sdk..."
        cd vendor/quickjs-ng

        # Build with wasi-sdk
        CC="$WASI_SDK/bin/clang" \
        AR="$WASI_SDK/bin/llvm-ar" \
        CFLAGS="--sysroot=$WASI_SDK/share/wasi-sysroot -O2 -DCONFIG_VERSION=\\\"2024\\\" -D_WASI_EMULATED_SIGNAL" \
        make qjs 2>&1 || {
            warn "QuickJS WASM build failed"
            cd "$SCRIPT_DIR"
        }

        if [ -f "qjs" ]; then
            mv qjs "$SCRIPT_DIR/edgebox-base.wasm"
            log "QuickJS WASM built successfully"
        fi
        cd "$SCRIPT_DIR"
    fi

    # If still no WASM, create a minimal placeholder
    if [ ! -f "edgebox-base.wasm" ]; then
        warn "No wasi-sdk found, creating minimal WASM placeholder..."
        warn "For full QuickJS WASM, install wasi-sdk:"
        warn "  macOS: brew install --cask aspect/packages/wasi-sdk"
        warn "  Linux: https://github.com/WebAssembly/wasi-sdk/releases"

        # Create a minimal WASM that prints a message
        cat > /tmp/edgebox-placeholder.wat << 'WATEOF'
(module
  (import "wasi_snapshot_preview1" "fd_write"
    (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (import "wasi_snapshot_preview1" "proc_exit"
    (func $proc_exit (param i32)))

  (memory (export "memory") 1)

  (data (i32.const 0) "EdgeBox: QuickJS WASM not built. Install wasi-sdk and run ./build.sh\n")

  (func (export "_start")
    (i32.store (i32.const 100) (i32.const 0))
    (i32.store (i32.const 104) (i32.const 70))
    (call $fd_write (i32.const 1) (i32.const 100) (i32.const 1) (i32.const 200))
    drop
    (call $proc_exit (i32.const 1))
  )
)
WATEOF

        if command -v wat2wasm &> /dev/null; then
            wat2wasm /tmp/edgebox-placeholder.wat -o edgebox-base.wasm
            rm /tmp/edgebox-placeholder.wat
        else
            warn "wat2wasm not found - no WASM generated"
            warn "Install wabt: brew install wabt (macOS) or apt install wabt (Linux)"
            rm /tmp/edgebox-placeholder.wat
        fi
    fi
fi

# Step 6: AOT compile if requested and available
if [ "$NO_AOT" = false ] && [ "$HAS_WASMEDGE" = true ] && [ -f "edgebox-base.wasm" ]; then
    log "AOT compiling with WasmEdge..."
    $WASMEDGEC edgebox-base.wasm edgebox.aot 2>&1 || {
        warn "AOT compilation failed, WASM module still usable"
    }

    if [ -f "edgebox.aot" ]; then
        AOT_SIZE=$(wc -c < edgebox.aot | tr -d ' ')
        log "AOT compiled: edgebox.aot ($(echo "scale=2; $AOT_SIZE/1024/1024" | bc)MB)"
    fi
elif [ "$NO_AOT" = false ] && [ "$HAS_WASMEDGE" = false ]; then
    warn "WasmEdge not found, skipping AOT compilation"
    warn "Install: curl -sSf https://raw.githubusercontent.com/WasmEdge/WasmEdge/master/utils/install.sh | bash"
fi

# Step 7: Summary
echo ""
log "=== Build Complete ==="
echo ""
echo "Generated files:"
[ -f "bundle.js" ] && echo "  - bundle.js          Claude Code CLI ($(echo "scale=2; $(wc -c < bundle.js | tr -d ' ')/1024/1024" | bc)MB)"
[ -f "tree-sitter.wasm" ] && echo "  - tree-sitter.wasm   Tree-sitter parser"
[ -f "edgebox-base.wasm" ] && echo "  - edgebox-base.wasm  QuickJS WASM runtime"
[ -f "edgebox.aot" ] && echo "  - edgebox.aot        AOT-compiled runtime"
[ -d "zig-out" ] && echo "  - zig-out/           Zig build output"
echo ""
echo "To run Claude Code:"
echo "  ./run.sh --claude \"Your prompt here\""
echo ""
echo "Or run JavaScript directly:"
echo "  ./run.sh script.js"
echo "  ./run.sh -e \"console.log('hello')\""
