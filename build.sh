#!/bin/bash
# EdgeBox Build Script
# Builds apps from examples/ or custom directories
#
# Usage:
#   ./build.sh                          # Build default (examples/hello)
#   ./build.sh examples/claude-code     # Build Claude Code example
#   ./build.sh my-app/                  # Build custom app directory
#   ./build.sh --clean                  # Clean and rebuild

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

log() { echo -e "${GREEN}[build]${NC} $1"; }
warn() { echo -e "${YELLOW}[warn]${NC} $1"; }
error() { echo -e "${RED}[error]${NC} $1"; exit 1; }

# Parse arguments
NO_AOT=false
CLEAN=false
APP_DIR=""
SKIP_BUNDLE=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --no-aot)
            NO_AOT=true
            shift
            ;;
        --clean)
            CLEAN=true
            shift
            ;;
        --help|-h)
            echo "EdgeBox Build Script"
            echo ""
            echo "Usage: ./build.sh [options] [app-directory]"
            echo ""
            echo "Options:"
            echo "  --no-aot      Skip AOT compilation (just build WASM)"
            echo "  --clean       Clean build artifacts before building"
            echo "  --help        Show this help"
            echo ""
            echo "Examples:"
            echo "  ./build.sh                          # Build default (hello)"
            echo "  ./build.sh examples/claude-code     # Build Claude Code"
            echo "  ./build.sh my-app/                  # Build custom app"
            echo ""
            echo "App Configuration (.edgebox.json):"
            echo "  {"
            echo "    \"npm\": \"package-name\",         // npm package to install"
            echo "    \"dirs\": [\"/tmp\", \"~/.app\"],    // directories to map"
            echo "    \"env\": [\"API_KEY\"]              // env vars to pass"
            echo "  }"
            exit 0
            ;;
        -*)
            error "Unknown option: $1"
            ;;
        *)
            APP_DIR="$1"
            shift
            ;;
    esac
done

# Clean if requested
if [ "$CLEAN" = true ]; then
    log "Cleaning build artifacts..."
    rm -rf *.wasm *.aot *.dylib *.so bundle.js .zig-cache zig-out
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

# Step 2: Determine app directory
if [ -z "$APP_DIR" ]; then
    # Default to examples/hello
    if [ -d "examples/hello" ]; then
        APP_DIR="examples/hello"
    else
        error "No app directory specified and examples/hello not found"
    fi
fi

if [ ! -d "$APP_DIR" ]; then
    error "App directory not found: $APP_DIR"
fi

log "App directory: $APP_DIR"

# Step 3: Read .edgebox.json config
CONFIG_FILE="$APP_DIR/.edgebox.json"
NPM_PACKAGE=""
APP_ENTRY=""

if [ -f "$CONFIG_FILE" ]; then
    log "Reading config: $CONFIG_FILE"

    # Parse npm package from config
    NPM_PACKAGE=$(bun -e "
        const cfg = require('./$CONFIG_FILE');
        if (cfg.npm) console.log(cfg.npm);
    " 2>/dev/null || true)

    if [ -n "$NPM_PACKAGE" ]; then
        log "npm package: $NPM_PACKAGE"
    fi
fi

# Step 4: Install npm package if specified
if [ -n "$NPM_PACKAGE" ]; then
    log "Installing npm package: $NPM_PACKAGE"

    # Create/update package.json in app directory
    if [ ! -f "$APP_DIR/package.json" ]; then
        echo '{"name":"edgebox-app","type":"module","private":true}' > "$APP_DIR/package.json"
    fi

    (cd "$APP_DIR" && bun add "$NPM_PACKAGE") 2>&1 || error "Failed to install $NPM_PACKAGE"

    # Find entry point from installed package
    PACKAGE_NAME=$(echo "$NPM_PACKAGE" | sed 's/@[0-9].*$//')
    PKG_DIR="$APP_DIR/node_modules/$PACKAGE_NAME"
    PKG_JSON="$PKG_DIR/package.json"

    if [ ! -f "$PKG_JSON" ]; then
        error "Package not found: $PKG_JSON"
    fi

    # Find entry point (bin > main > module > exports)
    ENTRY_INFO=$(bun -e "
        const pkg = require('./$PKG_JSON');
        if (pkg.bin) {
            const bin = typeof pkg.bin === 'string' ? pkg.bin : Object.values(pkg.bin)[0];
            if (bin) { console.log('bin:' + bin); process.exit(0); }
        }
        if (pkg.main) { console.log('main:' + pkg.main); process.exit(0); }
        if (pkg.module) { console.log('module:' + pkg.module); process.exit(0); }
        if (pkg.exports) {
            const exp = typeof pkg.exports === 'string' ? pkg.exports :
                        pkg.exports['.'] ? (typeof pkg.exports['.'] === 'string' ? pkg.exports['.'] : pkg.exports['.'].default || pkg.exports['.'].import || pkg.exports['.'].require) :
                        pkg.exports.default || pkg.exports.import || pkg.exports.require;
            if (exp) { console.log('exports:' + exp); process.exit(0); }
        }
        console.log('default:index.js');
    " 2>/dev/null)

    ENTRY_TYPE="${ENTRY_INFO%%:*}"
    ENTRY_FILE="${ENTRY_INFO#*:}"
    APP_ENTRY="$PKG_DIR/$ENTRY_FILE"

    log "Found entry point ($ENTRY_TYPE): $APP_ENTRY"

    # Check if file uses ESM (import/export) - needs re-bundling for QuickJS
    if head -100 "$APP_ENTRY" | grep -qE 'import\{|import |from"node:|import\.meta'; then
        log "ESM detected - will re-bundle for QuickJS"
        SKIP_BUNDLE=false
    elif [ "$ENTRY_TYPE" = "bin" ]; then
        log "CLI package (CommonJS) - using pre-bundled entry"
        cp "$APP_ENTRY" bundle.js
        SKIP_BUNDLE=true
    fi
else
    # No npm package - look for index.js in app directory
    if [ -f "$APP_DIR/index.js" ]; then
        APP_ENTRY="$APP_DIR/index.js"
    elif [ -f "$APP_DIR/main.js" ]; then
        APP_ENTRY="$APP_DIR/main.js"
    elif [ -f "$APP_DIR/app.js" ]; then
        APP_ENTRY="$APP_DIR/app.js"
    else
        error "No entry point found in $APP_DIR (index.js, main.js, or app.js)"
    fi
    log "Entry point: $APP_ENTRY"
fi

# Step 5: Bundle with Bun (skip if already bundled)
if [ "$SKIP_BUNDLE" = false ]; then
    log "Bundling with Bun..."
    # Use --format=cjs to output CommonJS (QuickJS doesn't support ESM import syntax)
    bun build "$APP_ENTRY" --outfile=bundle.js --target=node --format=cjs --minify 2>&1 || {
        warn "Bun minify failed, trying without..."
        bun build "$APP_ENTRY" --outfile=bundle.js --target=node --format=cjs
    }
fi

if [ -f "bundle.js" ]; then
    BUNDLE_SIZE=$(wc -c < bundle.js | tr -d ' ')
    log "Bundle: bundle.js ($(echo "scale=2; $BUNDLE_SIZE/1024" | bc)KB)"
fi

# Step 6: Prepend runtime polyfills to bundle (for unified bytecode cache)
POLYFILLS_FILE="$SCRIPT_DIR/src/polyfills/runtime.js"
if [ -f "bundle.js" ] && [ -f "$POLYFILLS_FILE" ]; then
    log "Prepending runtime polyfills to bundle..."
    # Create combined bundle: polyfills + semicolon + user code
    cat "$POLYFILLS_FILE" > bundle-with-polyfills.js
    echo ";" >> bundle-with-polyfills.js
    cat bundle.js >> bundle-with-polyfills.js
    mv bundle-with-polyfills.js bundle.js

    BUNDLE_SIZE=$(wc -c < bundle.js | tr -d ' ')
    log "Bundle with polyfills: bundle.js ($(echo "scale=2; $BUNDLE_SIZE/1024" | bc)KB)"
fi

# Step 7: Pre-compile bytecode cache (for instant startup)
# This runs the WASM once with --compile-only to generate bundle.js.cache
if [ -f "bundle.js" ] && [ -f "edgebox-base.wasm" ]; then
    log "Pre-compiling bytecode cache..."

    # Find WasmEdge
    WASMEDGE_BIN=""
    if command -v wasmedge &> /dev/null; then
        WASMEDGE_BIN="wasmedge"
    elif [ -f "$HOME/.wasmedge/bin/wasmedge" ]; then
        WASMEDGE_BIN="$HOME/.wasmedge/bin/wasmedge"
    fi

    if [ -n "$WASMEDGE_BIN" ]; then
        # Run with --compile-only flag to just generate cache without executing
        BUNDLE_ABS="$(pwd)/bundle.js"
        $WASMEDGE_BIN --dir "$(pwd)" edgebox-base.wasm --compile-only "$BUNDLE_ABS" 2>&1 || {
            warn "Bytecode pre-compilation failed (will compile on first run)"
        }

        if [ -f "bundle.js.cache" ]; then
            CACHE_SIZE=$(wc -c < bundle.js.cache | tr -d ' ')
            log "Bytecode cache: bundle.js.cache ($(echo "scale=2; $CACHE_SIZE/1024" | bc)KB)"
        fi
    fi
fi

# Step 7: Copy .edgebox.json to root for run.sh to use (or remove if none)
if [ -f "$CONFIG_FILE" ]; then
    cp "$CONFIG_FILE" .edgebox.json
    log "Config copied to .edgebox.json"
elif [ -f ".edgebox.json" ]; then
    rm .edgebox.json
    log "Removed old .edgebox.json (app has no config)"
fi

# Step 7: Build WASM module with Zig (if not already built)
if [ ! -f "edgebox-base.wasm" ]; then
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
        if zig build wasm -Doptimize=ReleaseFast 2>&1; then
            if [ -f "zig-out/bin/edgebox-base.wasm" ]; then
                cp zig-out/bin/edgebox-base.wasm edgebox-base.wasm
                WASM_SIZE=$(wc -c < edgebox-base.wasm | tr -d ' ')
                log "QuickJS WASM built: edgebox-base.wasm ($(echo "scale=2; $WASM_SIZE/1024" | bc)KB)"

                # Step 7b: Wizer pre-initialization (instant startup)
                # This snapshots the QuickJS runtime+context at build time
                # Must run BEFORE wasm-opt (wasm-opt strips the snapshotted memory)
                if command -v wizer &> /dev/null; then
                    log "Running Wizer pre-initialization..."
                    if wizer edgebox-base.wasm \
                        -o edgebox-wizer.wasm \
                        --allow-wasi \
                        --wasm-bulk-memory true \
                        --init-func wizer_init 2>/dev/null; then
                        mv edgebox-wizer.wasm edgebox-base.wasm
                        WIZER_SIZE=$(wc -c < edgebox-base.wasm | tr -d ' ')
                        log "Wizer snapshot: edgebox-base.wasm ($(echo "scale=2; $WIZER_SIZE/1024" | bc)KB)"
                    else
                        warn "Wizer pre-initialization failed (will use slower init path)"
                    fi
                else
                    log "Wizer not found - install with: cargo install wizer --features=\"env_logger structopt\""
                fi

                # Optimize with wasm-opt AFTER Wizer for smaller binary and faster load
                if command -v wasm-opt &> /dev/null; then
                    log "Optimizing WASM with wasm-opt..."
                    if wasm-opt -Oz --enable-simd --strip-debug edgebox-base.wasm -o edgebox-base-opt.wasm 2>/dev/null; then
                        mv edgebox-base-opt.wasm edgebox-base.wasm
                        OPT_SIZE=$(wc -c < edgebox-base.wasm | tr -d ' ')
                        log "Optimized WASM: $(echo "scale=2; $OPT_SIZE/1024" | bc)KB"
                    fi
                fi
            fi
        else
            warn "Zig WASM build failed"
        fi
    fi
else
    log "Using existing edgebox-base.wasm"
fi

# Step 8: AOT compile
HAS_WASMEDGE=false
WASMEDGE=""
if command -v wasmedge &> /dev/null; then
    WASMEDGE="wasmedge"
    HAS_WASMEDGE=true
elif [ -f "$HOME/.wasmedge/bin/wasmedge" ]; then
    WASMEDGE="$HOME/.wasmedge/bin/wasmedge"
    HAS_WASMEDGE=true
fi

case "$(uname -s)" in
    Darwin) AOT_EXT="dylib" ;;
    Linux)  AOT_EXT="so" ;;
    *)      AOT_EXT="so" ;;
esac

if [ "$NO_AOT" = false ] && [ "$HAS_WASMEDGE" = true ] && [ -f "edgebox-base.wasm" ]; then
    if [ ! -f "edgebox-aot.$AOT_EXT" ]; then
        log "AOT compiling with WasmEdge..."
        $WASMEDGE compile edgebox-base.wasm edgebox-aot.$AOT_EXT 2>&1 || {
            warn "AOT compilation failed, WASM module still usable"
        }

        if [ -f "edgebox-aot.$AOT_EXT" ]; then
            AOT_SIZE=$(wc -c < "edgebox-aot.$AOT_EXT" | tr -d ' ')
            log "AOT compiled: edgebox-aot.$AOT_EXT ($(echo "scale=2; $AOT_SIZE/1024/1024" | bc)MB)"
        fi
    else
        log "Using existing edgebox-aot.$AOT_EXT"
    fi
elif [ "$NO_AOT" = false ] && [ "$HAS_WASMEDGE" = false ]; then
    warn "WasmEdge not found, skipping AOT compilation"
fi

# Summary
echo ""
log "=== Build Complete ==="
echo ""
echo "Generated files:"
[ -f "bundle.js" ] && echo "  - bundle.js           App bundle ($(echo "scale=2; $(wc -c < bundle.js | tr -d ' ')/1024" | bc)KB)"
[ -f ".edgebox.json" ] && echo "  - .edgebox.json       App config"
[ -f "edgebox-base.wasm" ] && echo "  - edgebox-base.wasm   QuickJS WASM runtime"
[ -f "edgebox-aot.$AOT_EXT" ] && echo "  - edgebox-aot.$AOT_EXT   AOT-compiled"
echo ""
echo "To run:"
echo "  ./run.sh                    # Run the app"
echo "  ./run.sh -- --help          # Pass args to the app"
echo ""
