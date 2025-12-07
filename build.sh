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

if ! command -v bun &> /dev/null; then
    error "Bun is required. Install with: curl -fsSL https://bun.sh/install | bash"
fi
log "Bun $(bun --version)"

if ! command -v zig &> /dev/null; then
    warn "Zig not found - skipping WASM build"
    HAS_ZIG=false
else
    HAS_ZIG=true
    log "Zig $(zig version)"
fi

# Step 2: Bundle examples/hello/index.js as default app
log "Bundling default example..."

# Use examples/hello as default if no app/app.js
APP_ENTRY=""
if [ -f "app/app.js" ]; then
    APP_ENTRY="app/app.js"
elif [ -f "examples/hello/index.js" ]; then
    APP_ENTRY="examples/hello/index.js"
else
    warn "No app found, creating examples/hello..."
    mkdir -p examples/hello
    cat > examples/hello/index.js << 'APPEOF'
// Hello World Example
print("Hello from EdgeBox!");
print("Platform:", os.platform());
APPEOF
    APP_ENTRY="examples/hello/index.js"
fi

log "Entry point: $APP_ENTRY"

# Bundle with bun
log "Bundling with Bun..."
bun build "$APP_ENTRY" --outfile=bundle.js --target=browser --minify 2>&1 || {
    warn "Bun minify failed, trying without..."
    bun build "$APP_ENTRY" --outfile=bundle.js --target=browser
}

if [ -f "bundle.js" ]; then
    BUNDLE_SIZE=$(wc -c < bundle.js | tr -d ' ')
    log "Bundle created: bundle.js ($(echo "scale=2; $BUNDLE_SIZE/1024" | bc)KB)"
fi

# Step 4: Build WASM module with Zig
log "Building QuickJS WASM with Zig..."

if [ "$HAS_ZIG" = false ]; then
    echo ""
    warn "============================================"
    warn "Zig not found - QuickJS WASM cannot be built"
    warn "============================================"
    echo ""
    warn "Install Zig to build QuickJS WASM:"
    echo ""
    echo "  macOS:  brew install zig"
    echo "  Linux:  https://ziglang.org/download/"
    echo ""
    warn "Then run: ./build.sh --clean"
    echo ""
else
    # Build WASM with Zig (cross-compiles QuickJS C to wasm32-wasi)
    if zig build wasm -Doptimize=ReleaseSmall 2>&1; then
        if [ -f "zig-out/bin/edgebox-base.wasm" ]; then
            cp zig-out/bin/edgebox-base.wasm edgebox-base.wasm
            WASM_SIZE=$(wc -c < edgebox-base.wasm | tr -d ' ')
            log "QuickJS WASM built: edgebox-base.wasm ($(echo "scale=2; $WASM_SIZE/1024" | bc)KB)"
        fi
    else
        warn "Zig WASM build failed"
    fi
fi

# Check if we have wasmedge for AOT compilation
HAS_WASMEDGE=false
WASMEDGE=""
if command -v wasmedge &> /dev/null; then
    WASMEDGE="wasmedge"
    HAS_WASMEDGE=true
elif [ -f "$HOME/.wasmedge/bin/wasmedge" ]; then
    WASMEDGE="$HOME/.wasmedge/bin/wasmedge"
    HAS_WASMEDGE=true
fi

# Step 6: AOT compile if requested and available
# WasmEdge uses "wasmedge compile" for AOT (replaces old wasmedgec)
if [ "$NO_AOT" = false ] && [ "$HAS_WASMEDGE" = true ] && [ -f "edgebox-base.wasm" ]; then
    log "AOT compiling with WasmEdge..."

    # Determine output extension based on platform
    case "$(uname -s)" in
        Darwin) AOT_EXT="dylib" ;;
        Linux)  AOT_EXT="so" ;;
        *)      AOT_EXT="so" ;;
    esac

    $WASMEDGE compile edgebox-base.wasm edgebox-aot.$AOT_EXT 2>&1 || {
        warn "AOT compilation failed, WASM module still usable"
    }

    if [ -f "edgebox-aot.$AOT_EXT" ]; then
        AOT_SIZE=$(wc -c < "edgebox-aot.$AOT_EXT" | tr -d ' ')
        log "AOT compiled: edgebox-aot.$AOT_EXT ($(echo "scale=2; $AOT_SIZE/1024/1024" | bc)MB)"
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
[ -f "bundle.js" ] && echo "  - bundle.js           App bundle ($(echo "scale=2; $(wc -c < bundle.js | tr -d ' ')/1024" | bc)KB)"
[ -f "edgebox-base.wasm" ] && echo "  - edgebox-base.wasm   QuickJS WASM runtime ($(echo "scale=2; $(wc -c < edgebox-base.wasm | tr -d ' ')/1024" | bc)KB)"
[ -f "edgebox-aot.dylib" ] && echo "  - edgebox-aot.dylib   AOT-compiled (macOS) ($(echo "scale=2; $(wc -c < edgebox-aot.dylib | tr -d ' ')/1024/1024" | bc)MB)"
[ -f "edgebox-aot.so" ] && echo "  - edgebox-aot.so      AOT-compiled (Linux) ($(echo "scale=2; $(wc -c < edgebox-aot.so | tr -d ' ')/1024/1024" | bc)MB)"
[ -d "zig-out" ] && echo "  - zig-out/            Zig build output"
echo ""
echo "To run examples:"
echo "  ./run.sh                                    # Run default (hello)"
echo "  ./run.sh examples/claude-code/index.js      # Run Claude Code"
echo "  ./run.sh examples/hello/index.js            # Run hello world"
echo ""
echo "Or run JavaScript directly:"
echo "  ./run.sh script.js"
echo "  ./run.sh -e \"print('hello')\""
