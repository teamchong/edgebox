#!/bin/bash
# EdgeBox Benchmark Comparison
# Compares EdgeBox against wasmedge-quickjs, Bun, Node.js, and Porffor

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR/.."

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}                EdgeBox Benchmark Comparison${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
echo ""

# Check dependencies
check_cmd() {
    if ! command -v "$1" &> /dev/null; then
        echo "Missing: $1"
        return 1
    fi
}

check_cmd wasmedge || exit 1
check_cmd hyperfine || { echo "Install: brew install hyperfine"; exit 1; }
check_cmd bun || echo "Warning: bun not found, skipping"
check_cmd node || echo "Warning: node not found, skipping"
check_cmd porf || echo "Warning: porf not found, skipping"

# Find wasmedge-quickjs AOT
WASMEDGE_QJS="$HOME/.wasmedge/lib/wasmedge_quickjs_aot.wasm"
if [ ! -f "$WASMEDGE_QJS" ]; then
    echo "Warning: wasmedge-quickjs not found at $WASMEDGE_QJS"
    echo "Install: https://github.com/aspect-build/aspect-workflows/releases"
fi

# Build EdgeBox if needed
if [ ! -f "edgebox-aot.dylib" ] && [ ! -f "edgebox-aot.so" ]; then
    echo "Building EdgeBox..."
    ./build.sh > /dev/null 2>&1
fi

# Determine AOT file
AOT_FILE="edgebox-aot.dylib"
[ -f "edgebox-aot.so" ] && AOT_FILE="edgebox-aot.so"

# Create temp fib file for CPU test
echo 'function fib(n){return n<2?n:fib(n-1)+fib(n-2)} console.log(fib(35))' > /tmp/fib35.js

# Build command arrays
EDGEBOX_CMD="wasmedge --dir . $AOT_FILE"
WASMEDGE_QJS_CMD="wasmedge --dir . $WASMEDGE_QJS"

echo -e "${GREEN}>>> Cold Start (hello.js)${NC}"
echo ""
hyperfine --warmup 3 --runs 20 \
    "$EDGEBOX_CMD bench/hello.js" \
    "$WASMEDGE_QJS_CMD bench/hello.js" \
    "bun bench/hello.js" \
    "node bench/hello.js" \
    "porf bench/hello.js" \
    --command-name "EdgeBox" \
    --command-name "wasmedge-quickjs" \
    --command-name "Bun" \
    --command-name "Node.js" \
    --command-name "Porffor" \
    2>&1 || true

echo ""
echo -e "${GREEN}>>> Allocator Stress (alloc_stress.js)${NC}"
echo ""
hyperfine --warmup 2 --runs 10 \
    "$EDGEBOX_CMD bench/alloc_stress.js" \
    "$WASMEDGE_QJS_CMD bench/alloc_stress.js" \
    "bun bench/alloc_stress.js" \
    "node bench/alloc_stress.js" \
    "porf bench/alloc_stress.js" \
    --command-name "EdgeBox" \
    --command-name "wasmedge-quickjs" \
    --command-name "Bun" \
    --command-name "Node.js" \
    --command-name "Porffor" \
    2>&1 || true

echo ""
echo -e "${GREEN}>>> CPU Test: fib(35)${NC}"
echo ""
hyperfine --warmup 1 --runs 5 \
    "wasmedge --dir /tmp:/private/tmp $AOT_FILE /tmp/fib35.js" \
    "wasmedge --dir /tmp:/private/tmp $WASMEDGE_QJS /tmp/fib35.js" \
    "bun /tmp/fib35.js" \
    "node /tmp/fib35.js" \
    "porf /tmp/fib35.js" \
    --command-name "EdgeBox" \
    --command-name "wasmedge-quickjs" \
    --command-name "Bun" \
    --command-name "Node.js" \
    --command-name "Porffor" \
    2>&1 || true

echo ""
echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}                         Done${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
