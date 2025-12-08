#!/bin/bash
# EdgeBox Cold Start Benchmark
# Compares edgebox runner vs wasmedge CLI

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
EDGEBOX="$ROOT_DIR/zig-out/bin/edgebox"

# Check for hyperfine
if ! command -v hyperfine &> /dev/null; then
    echo "Installing hyperfine..."
    brew install hyperfine
fi

# Check for edgebox
if [ ! -x "$EDGEBOX" ]; then
    echo "Building edgebox runner..."
    cd "$ROOT_DIR" && zig build runner -Doptimize=ReleaseFast
fi

# Use existing AOT dylib or build one
DYLIB="$SCRIPT_DIR/hello.dylib"
if [ ! -f "$DYLIB" ]; then
    echo "Building hello.dylib..."
    wasmedgec "$SCRIPT_DIR/hello.wasm" "$DYLIB" 2>/dev/null || true
fi

echo "═══════════════════════════════════════════════════════════════"
echo "                    EdgeBox Cold Start Benchmark"
echo "═══════════════════════════════════════════════════════════════"
echo ""

if [ -f "$DYLIB" ]; then
    hyperfine --warmup 3 --runs 20 \
        "$EDGEBOX $DYLIB" \
        "wasmedge $DYLIB" \
        "bun $SCRIPT_DIR/hello.js" \
        "node $SCRIPT_DIR/hello.js"
else
    echo "No dylib found, using wasm..."
    hyperfine --warmup 3 --runs 20 \
        "$EDGEBOX $SCRIPT_DIR/hello.wasm" \
        "wasmedge $SCRIPT_DIR/hello.wasm" \
        "bun $SCRIPT_DIR/hello.js" \
        "node $SCRIPT_DIR/hello.js"
fi
