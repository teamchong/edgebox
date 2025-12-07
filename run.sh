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
CLAUDE_PROMPT=""
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
        --claude)
            CLAUDE_PROMPT="$2"
            shift 2
            ;;
        --help|-h)
            echo -e "${CYAN}EdgeBox${NC} - QuickJS + WASI + WasmEdge AOT Runtime"
            echo ""
            echo "Usage: ./run.sh [options] [script.js] [args...]"
            echo ""
            echo "Options:"
            echo "  --aot              Use AOT-compiled version (faster startup)"
            echo "  -e, --eval CODE    Evaluate JavaScript code"
            echo "  --claude PROMPT    Run Claude Code with a prompt"
            echo "  --help             Show this help"
            echo ""
            echo "Examples:"
            echo "  ./run.sh                           # Interactive mode"
            echo "  ./run.sh script.js                 # Run a script"
            echo "  ./run.sh -e \"console.log('hi')\"    # Evaluate code"
            echo "  ./run.sh --aot script.js           # Run with AOT"
            echo "  ./run.sh --claude \"Fix the bug\"    # Run Claude Code"
            echo ""
            echo "Environment:"
            echo "  ANTHROPIC_API_KEY   Required for Claude Code"
            echo "  WASMEDGE_DIR        WasmEdge installation path"
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

# Build WasmEdge arguments
WASM_ARGS=()

# Directory mappings for WASI
WASM_ARGS+=("--dir" ".:.")
WASM_ARGS+=("--dir" "/tmp:/tmp")

# Map home directory for config files
if [ -n "$HOME" ]; then
    WASM_ARGS+=("--dir" "$HOME:$HOME")
fi

# Map current working directory
CWD="$(pwd)"
if [ "$CWD" != "." ]; then
    WASM_ARGS+=("--dir" "$CWD:$CWD")
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

# Handle different run modes
if [ -n "$CLAUDE_PROMPT" ]; then
    # Claude Code mode
    if [ -z "$ANTHROPIC_API_KEY" ]; then
        error "ANTHROPIC_API_KEY environment variable is required for Claude Code"
    fi

    log "Running Claude Code..."

    # Create a wrapper script that loads bundle.js and runs Claude
    WRAPPER=$(mktemp /tmp/edgebox-claude-XXXXXX.js)
    cat > "$WRAPPER" << EOF
// Load the bundled Claude Code
const bundle = require('./bundle.js');

// Run Claude with the prompt
async function main() {
    try {
        const result = await globalThis.claudeCode.run({
            prompt: ${CLAUDE_PROMPT@Q},
            workdir: process.cwd()
        });
        console.log(JSON.stringify(result, null, 2));
    } catch (err) {
        console.error('Error:', err.message);
        process.exit(1);
    }
}

main();
EOF

    WASM_ARGS+=("$WRAPPER")
    WASM_ARGS+=("${EXTRA_ARGS[@]}")

    $WASMEDGE "${WASM_ARGS[@]}"
    rm -f "$WRAPPER"

elif [ -n "$EVAL_CODE" ]; then
    # Eval mode
    log "Evaluating: $EVAL_CODE"

    WRAPPER=$(mktemp /tmp/edgebox-eval-XXXXXX.js)
    cat > "$WRAPPER" << EOF
const result = eval(${EVAL_CODE@Q});
if (result !== undefined) {
    console.log(result);
}
EOF

    WASM_ARGS+=("$WRAPPER")

    $WASMEDGE "${WASM_ARGS[@]}"
    rm -f "$WRAPPER"

elif [ -n "$SCRIPT_FILE" ]; then
    # Script file mode
    if [ ! -f "$SCRIPT_FILE" ]; then
        error "Script not found: $SCRIPT_FILE"
    fi

    log "Running: $SCRIPT_FILE"
    WASM_ARGS+=("$SCRIPT_FILE")
    WASM_ARGS+=("${EXTRA_ARGS[@]}")

    $WASMEDGE "${WASM_ARGS[@]}"

else
    # Interactive/REPL mode
    log "Starting interactive mode..."
    log "Module: $MODULE"
    echo ""

    # For now, just run the module which should show help
    $WASMEDGE "${WASM_ARGS[@]}"
fi
