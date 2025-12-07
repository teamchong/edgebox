#!/bin/bash
# EdgeBox Run Script
# Runs JavaScript in QuickJS WASM with WasmEdge
#
# Usage:
#   ./run.sh                        # Run bundle.js
#   ./run.sh script.js              # Run a JavaScript file
#   ./run.sh -e "code"              # Evaluate JavaScript code
#   ./run.sh -- --help              # Pass args to the JS app

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

error() { echo -e "\033[0;31m[error]\033[0m $1"; exit 1; }

# Parse arguments first to check for app directory
NO_AOT=false
EVAL_CODE=""
SCRIPT_FILE=""
APP_DIR=""
PASSTHROUGH_ARGS=()
PARSING_PASSTHROUGH=false

while [[ $# -gt 0 ]]; do
    if [ "$PARSING_PASSTHROUGH" = true ]; then
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
            echo "EdgeBox - QuickJS WASM Runtime"
            echo ""
            echo "Usage: ./run.sh [options] [app-dir|script.js] [-- args...]"
            echo ""
            echo "Options:"
            echo "  --no-aot           Use WASM instead of AOT"
            echo "  -e, --eval CODE    Evaluate JavaScript code"
            echo "  --help             Show this help"
            echo "  --                 Pass remaining args to the JS app"
            echo ""
            echo "Examples:"
            echo "  ./run.sh                       # Run bundle.js"
            echo "  ./run.sh examples/claude-code  # Build and run app"
            echo "  ./run.sh script.js             # Run a script"
            echo "  ./run.sh -e \"print(1+2)\"       # Evaluate code"
            exit 0
            ;;
        -*)
            PASSTHROUGH_ARGS+=("$1")
            shift
            ;;
        *)
            # Check if it's an app directory (has index.js or .edgebox.json)
            if [ -z "$APP_DIR" ] && [ -z "$SCRIPT_FILE" ] && [ -d "$1" ]; then
                if [ -f "$1/index.js" ] || [ -f "$1/.edgebox.json" ] || [ -f "$1/main.js" ]; then
                    APP_DIR="$1"
                else
                    PASSTHROUGH_ARGS+=("$1")
                fi
            elif [ -z "$SCRIPT_FILE" ] && [ -f "$1" ]; then
                SCRIPT_FILE="$1"
            else
                PASSTHROUGH_ARGS+=("$1")
            fi
            shift
            ;;
    esac
done

# Determine if we need to build
NEED_BUILD=false
LAST_BUILD_FILE=".last-build"

if [ ! -f "bundle.js" ] || [ ! -f "edgebox-base.wasm" ]; then
    NEED_BUILD=true
elif [ -n "$APP_DIR" ]; then
    # Check if app directory changed from last build
    LAST_APP=""
    [ -f "$LAST_BUILD_FILE" ] && LAST_APP=$(cat "$LAST_BUILD_FILE")

    if [ "$LAST_APP" != "$APP_DIR" ]; then
        NEED_BUILD=true
    fi
fi

if [ "$NEED_BUILD" = true ]; then
    BUILD_OUTPUT=$(mktemp)
    BUILD_CMD="./build.sh"
    [ -n "$APP_DIR" ] && BUILD_CMD="./build.sh $APP_DIR"

    if ! $BUILD_CMD > "$BUILD_OUTPUT" 2>&1; then
        cat "$BUILD_OUTPUT"
        rm -f "$BUILD_OUTPUT"
        exit 1
    fi
    rm -f "$BUILD_OUTPUT"

    if [ -n "$APP_DIR" ]; then
        echo "$APP_DIR" > "$LAST_BUILD_FILE"
    else
        echo "examples/hello" > "$LAST_BUILD_FILE"
    fi
fi

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

# Determine AOT file extension
case "$(uname -s)" in
    Darwin) AOT_FILE="edgebox-aot.dylib" ;;
    Linux)  AOT_FILE="edgebox-aot.so" ;;
    *)      AOT_FILE="edgebox-aot.so" ;;
esac

# Determine which module to run
MODULE=""
if [ "$NO_AOT" = true ]; then
    MODULE="edgebox-base.wasm"
elif [ -f "$AOT_FILE" ]; then
    MODULE="$AOT_FILE"
elif [ -f "edgebox-base.wasm" ]; then
    MODULE="edgebox-base.wasm"
fi

if [ -z "$MODULE" ] || [ ! -f "$MODULE" ]; then
    error "No WASM/AOT module found. Run ./build.sh first."
fi

# Check if placeholder WASM
MODULE_SIZE=$(wc -c < "$MODULE" | tr -d ' ')
if [ "$MODULE_SIZE" -lt 1000 ]; then
    error "QuickJS WASM not built. Install Zig and run: ./build.sh --clean"
fi

# Build WasmEdge arguments
WASM_ARGS=()

# Helper to add directory mapping
MAPPED_DIRS=()
add_dir_mapping() {
    local dir="$1"
    # Expand ~ to $HOME
    dir="${dir/#\~/$HOME}"

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

# Read .edgebox.json config if present
CONFIG_FILE=".edgebox.json"
if [ -f "$CONFIG_FILE" ]; then
    # Parse dirs array
    DIRS=$(bun -e "
        const cfg = require('./$CONFIG_FILE');
        if (cfg.dirs && Array.isArray(cfg.dirs)) {
            cfg.dirs.forEach(d => console.log(d));
        }
    " 2>/dev/null || true)

    while IFS= read -r dir; do
        [ -n "$dir" ] && add_dir_mapping "$dir"
    done <<< "$DIRS"

    # Parse env array
    ENV_VARS=$(bun -e "
        const cfg = require('./$CONFIG_FILE');
        if (cfg.env && Array.isArray(cfg.env)) {
            cfg.env.forEach(e => console.log(e));
        }
    " 2>/dev/null || true)

    while IFS= read -r var; do
        if [ -n "$var" ]; then
            val="${!var}"
            [ -n "$val" ] && WASM_ARGS+=("--env" "$var=$val")
        fi
    done <<< "$ENV_VARS"
else
    # Default mappings when no config
    add_dir_mapping "$(pwd)"
    add_dir_mapping "/tmp"
    [ -n "$HOME" ] && add_dir_mapping "$HOME"

    # Default env vars
    [ -n "$ANTHROPIC_API_KEY" ] && WASM_ARGS+=("--env" "ANTHROPIC_API_KEY=$ANTHROPIC_API_KEY")
    [ -n "$HOME" ] && WASM_ARGS+=("--env" "HOME=$HOME")
fi

# Always add PWD and PATH
CWD="$(pwd)"
add_dir_mapping "$CWD"
WASM_ARGS+=("--env" "PWD=$CWD")
WASM_ARGS+=("--env" "PATH=/usr/bin:/bin")

# Map script file's directory if provided
if [ -n "$SCRIPT_FILE" ] && [ -f "$SCRIPT_FILE" ]; then
    SCRIPT_DIR_PATH=$(dirname "$(realpath "$SCRIPT_FILE" 2>/dev/null || echo "$SCRIPT_FILE")")
    add_dir_mapping "$SCRIPT_DIR_PATH"
fi

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
