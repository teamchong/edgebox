#!/bin/bash
# EdgeBox Run Script
# Runs Claude Code in QuickJS WASM with WasmEdge
#
# Usage:
#   ./run.sh                     # Interactive REPL
#   ./run.sh script.js           # Run a JavaScript file
#   ./run.sh -e "code"           # Evaluate JavaScript code
#   ./run.sh --aot               # Use AOT-compiled version (faster startup)
#   ./run.sh --claude "prompt"   # Run Claude Code with a prompt

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

# Parse arguments
USE_AOT=false
EVAL_CODE=""
SCRIPT_FILE=""
EXTRA_ARGS=()

while [[ $# -gt 0 ]]; do
    case $1 in
        --aot)
            USE_AOT=true
            shift
            ;;
        -e|--eval)
            EVAL_CODE="$2"
            shift 2
            ;;
        --help|-h)
            echo -e "${CYAN}EdgeBox${NC} - QuickJS WASM Runtime (Zig + WasmEdge)"
            echo ""
            echo "Usage: ./run.sh [options] [script.js] [args...]"
            echo ""
            echo "Options:"
            echo "  --aot              Use AOT-compiled version (faster startup)"
            echo "  -e, --eval CODE    Evaluate JavaScript code"
            echo "  --help             Show this help"
            echo ""
            echo "Examples:"
            echo "  ./run.sh                           # Show help"
            echo "  ./run.sh script.js                 # Run a script"
            echo "  ./run.sh -e \"1 + 2\"                # Evaluate code"
            echo "  ./run.sh --aot script.js           # Run with AOT"
            exit 0
            ;;
        -*)
            EXTRA_ARGS+=("$1")
            shift
            ;;
        *)
            if [ -z "$SCRIPT_FILE" ]; then
                SCRIPT_FILE="$1"
            else
                EXTRA_ARGS+=("$1")
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

# Determine which module to run
MODULE=""
if [ "$USE_AOT" = true ]; then
    if [ -f "edgebox.aot" ]; then
        MODULE="edgebox.aot"
        log "Using AOT-compiled module"
    else
        warn "AOT module not found, falling back to WASM"
        MODULE="edgebox-base.wasm"
    fi
else
    if [ -f "edgebox-base.wasm" ]; then
        MODULE="edgebox-base.wasm"
    elif [ -f "edgebox.aot" ]; then
        MODULE="edgebox.aot"
        log "Using AOT module (WASM not found)"
    fi
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
# Tracks mapped dirs to avoid duplicates
MAPPED_DIRS=()
add_dir_mapping() {
    local dir="$1"
    if [ -d "$dir" ]; then
        local real_dir
        real_dir=$(realpath "$dir" 2>/dev/null || echo "$dir")
        # Check if already mapped
        for mapped in "${MAPPED_DIRS[@]}"; do
            [ "$mapped" = "$real_dir" ] && return
        done
        MAPPED_DIRS+=("$real_dir")
        WASM_ARGS+=("--dir" "$real_dir")
    fi
}

# Map current working directory (resolved to handle symlinks)
CWD="$(pwd)"
add_dir_mapping "$CWD"

# Map /tmp (used internally for temp wrapper files)
add_dir_mapping "/tmp"

# Map home directory for config files
if [ -n "$HOME" ]; then
    add_dir_mapping "$HOME"
fi

# Map script file's directory if provided
if [ -n "$SCRIPT_FILE" ] && [ -f "$SCRIPT_FILE" ]; then
    SCRIPT_DIR_PATH=$(dirname "$(realpath "$SCRIPT_FILE" 2>/dev/null || echo "$SCRIPT_FILE")")
    add_dir_mapping "$SCRIPT_DIR_PATH"
fi

# Environment variables
if [ -n "$ANTHROPIC_API_KEY" ]; then
    WASM_ARGS+=("--env" "ANTHROPIC_API_KEY=$ANTHROPIC_API_KEY")
fi

if [ -n "$HOME" ]; then
    WASM_ARGS+=("--env" "HOME=$HOME")
fi

WASM_ARGS+=("--env" "PWD=$CWD")
WASM_ARGS+=("--env" "PATH=/usr/bin:/bin")

# Add the module
WASM_ARGS+=("$MODULE")

# Pass arguments to WASM module

if [ -n "$EVAL_CODE" ]; then
    WASM_ARGS+=("-e" "$EVAL_CODE")
elif [ -n "$SCRIPT_FILE" ]; then
    if [ ! -f "$SCRIPT_FILE" ]; then
        error "Script not found: $SCRIPT_FILE"
    fi
    WASM_ARGS+=("$SCRIPT_FILE")
elif [ -f "bundle.js" ]; then
    # Default: run bundle.js (built from app/app.js)
    WASM_ARGS+=("bundle.js")
fi

WASM_ARGS+=("${EXTRA_ARGS[@]}")

$WASMEDGE "${WASM_ARGS[@]}"
