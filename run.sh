#!/bin/bash
# EdgeBox Run Script
# Runs JavaScript in QuickJS WASM with WasmEdge
#
# Usage:
#   ./run.sh                        # Run bundle.js (default)
#   ./run.sh script.js              # Run a JavaScript file
#   ./run.sh -e "code"              # Evaluate JavaScript code
#   ./run.sh -- -p "prompt"         # Pass args to the JS app
#   echo "data" | ./run.sh          # Stdin is passed to JS app

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

log() { echo -e "${GREEN}[edgebox]${NC} $1"; }
warn() { echo -e "${YELLOW}[warn]${NC} $1"; }
error() { echo -e "${RED}[error]${NC} $1"; exit 1; }

# Auto-run build.sh if not built yet
if [ ! -f "bundle.js" ] || [ ! -f "edgebox-base.wasm" ]; then
    log "Build artifacts not found, running build.sh..."
    ./build.sh
fi

# Parse arguments - stop at -- for passthrough
NO_AOT=false
EVAL_CODE=""
SCRIPT_FILE=""
PASSTHROUGH_ARGS=()
PARSING_PASSTHROUGH=false

while [[ $# -gt 0 ]]; do
    if [ "$PARSING_PASSTHROUGH" = true ]; then
        # Everything after -- goes to the JS app
        PASSTHROUGH_ARGS+=("$1")
        shift
        continue
    fi

    case $1 in
        --)
            PARSING_PASSTHROUGH=true
            shift
            ;;
        --no-aot)
            NO_AOT=true
            shift
            ;;
        -e|--eval)
            EVAL_CODE="$2"
            shift 2
            ;;
        --help|-h)
            echo -e "${CYAN}EdgeBox${NC} - QuickJS WASM Runtime (Zig + WasmEdge)"
            echo ""
            echo "Usage: ./run.sh [options] [script.js] [-- args...]"
            echo ""
            echo "Options:"
            echo "  --no-aot           Use WASM instead of AOT (slower)"
            echo "  -e, --eval CODE    Evaluate JavaScript code"
            echo "  --help             Show this help"
            echo "  --                 Pass remaining args to the JS app"
            echo ""
            echo "Examples:"
            echo "  ./run.sh                           # Run bundle.js"
            echo "  ./run.sh script.js                 # Run a script"
            echo "  ./run.sh -e \"print(1+2)\"           # Evaluate code"
            echo "  ./run.sh -- -p \"hello\"             # Pass -p to JS app"
            echo "  echo 'data' | ./run.sh             # Pipe data to JS stdin"
            exit 0
            ;;
        -*)
            # Unknown flag - pass through to JS
            PASSTHROUGH_ARGS+=("$1")
            shift
            ;;
        *)
            if [ -z "$SCRIPT_FILE" ] && [ -f "$1" ]; then
                SCRIPT_FILE="$1"
            else
                PASSTHROUGH_ARGS+=("$1")
            fi
            shift
            ;;
    esac
done

# Find WasmEdge
WASMEDGE=""
if command -v wasmedge &> /dev/null; then
    WASMEDGE="wasmedge"
elif [ -f "$HOME/.wasmedge/bin/wasmedge" ]; then
    WASMEDGE="$HOME/.wasmedge/bin/wasmedge"
elif [ -n "$WASMEDGE_DIR" ] && [ -f "$WASMEDGE_DIR/bin/wasmedge" ]; then
    WASMEDGE="$WASMEDGE_DIR/bin/wasmedge"
else
    error "WasmEdge not found. Install with:
  curl -sSf https://raw.githubusercontent.com/WasmEdge/WasmEdge/master/utils/install.sh | bash"
fi

log "Using WasmEdge: $WASMEDGE"

# Determine AOT file extension based on platform
case "$(uname -s)" in
    Darwin) AOT_FILE="edgebox-aot.dylib" ;;
    Linux)  AOT_FILE="edgebox-aot.so" ;;
    *)      AOT_FILE="edgebox-aot.so" ;;
esac

# Determine which module to run
MODULE=""
if [ "$NO_AOT" = true ]; then
    MODULE="edgebox-base.wasm"
    log "Using WASM module (--no-aot)"
elif [ -f "$AOT_FILE" ]; then
    MODULE="$AOT_FILE"
    log "Using AOT-compiled module (faster)"
elif [ -f "edgebox-base.wasm" ]; then
    MODULE="edgebox-base.wasm"
    log "Using WASM module"
fi

if [ -z "$MODULE" ] || [ ! -f "$MODULE" ]; then
    error "No WASM/AOT module found. Run ./build.sh first."
fi

# Check if using placeholder WASM (< 1KB means it's the placeholder)
MODULE_SIZE=$(wc -c < "$MODULE" | tr -d ' ')
if [ "$MODULE_SIZE" -lt 1000 ]; then
    echo ""
    warn "============================================"
    warn "QuickJS WASM not built"
    warn "============================================"
    echo ""
    warn "Install Zig to build QuickJS WASM:"
    echo ""
    echo "  macOS:  brew install zig"
    echo "  Linux:  https://ziglang.org/download/"
    echo ""
    warn "Then run: ./build.sh --clean"
    echo ""
    exit 1
fi

# Build WasmEdge arguments
WASM_ARGS=()

# Helper to resolve symlinks and add directory mapping
MAPPED_DIRS=()
add_dir_mapping() {
    local dir="$1"
    if [ -d "$dir" ]; then
        local real_dir
        real_dir=$(realpath "$dir" 2>/dev/null || echo "$dir")
        for mapped in "${MAPPED_DIRS[@]}"; do
            [ "$mapped" = "$real_dir" ] && return
        done
        MAPPED_DIRS+=("$real_dir")
        WASM_ARGS+=("--dir" "$real_dir")
    fi
}

# Map directories
CWD="$(pwd)"
add_dir_mapping "$CWD"
add_dir_mapping "/tmp"
[ -n "$HOME" ] && add_dir_mapping "$HOME"

# Map script file's directory if provided
if [ -n "$SCRIPT_FILE" ] && [ -f "$SCRIPT_FILE" ]; then
    SCRIPT_DIR_PATH=$(dirname "$(realpath "$SCRIPT_FILE" 2>/dev/null || echo "$SCRIPT_FILE")")
    add_dir_mapping "$SCRIPT_DIR_PATH"
fi

# Environment variables
[ -n "$ANTHROPIC_API_KEY" ] && WASM_ARGS+=("--env" "ANTHROPIC_API_KEY=$ANTHROPIC_API_KEY")
[ -n "$HOME" ] && WASM_ARGS+=("--env" "HOME=$HOME")
WASM_ARGS+=("--env" "PWD=$CWD")
WASM_ARGS+=("--env" "PATH=/usr/bin:/bin")

# Add the module
WASM_ARGS+=("$MODULE")

# Determine what to run
if [ -n "$EVAL_CODE" ]; then
    WASM_ARGS+=("-e" "$EVAL_CODE")
elif [ -n "$SCRIPT_FILE" ]; then
    SCRIPT_FILE_ABS=$(realpath "$SCRIPT_FILE" 2>/dev/null || echo "$SCRIPT_FILE")
    WASM_ARGS+=("$SCRIPT_FILE_ABS")
elif [ -f "bundle.js" ]; then
    BUNDLE_ABS=$(realpath "bundle.js" 2>/dev/null || echo "$SCRIPT_DIR/bundle.js")
    WASM_ARGS+=("$BUNDLE_ABS")
fi

# Add passthrough arguments
WASM_ARGS+=("${PASSTHROUGH_ARGS[@]}")

exec $WASMEDGE "${WASM_ARGS[@]}"
