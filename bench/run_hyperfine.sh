#!/bin/bash
# EdgeBox Benchmark Suite
# Compares edgebox vs bun vs wasmedge-qjs vs node vs porffor

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
EDGEBOX="$ROOT_DIR/zig-out/bin/edgebox"

# Check for hyperfine
if ! command -v hyperfine &> /dev/null; then
    echo "Installing hyperfine..."
    brew install hyperfine
fi

# Check for edgebox CLI
if [ ! -x "$EDGEBOX" ]; then
    echo "Building edgebox CLI..."
    cd "$ROOT_DIR" && zig build cli
fi

# Setup wasmedge-quickjs
WASMEDGE_QJS=""
if [ -f "$HOME/.wasmedge/lib/wasmedge_quickjs_aot.wasm" ]; then
    WASMEDGE_QJS="$HOME/.wasmedge/lib/wasmedge_quickjs_aot.wasm"
elif [ -f "$HOME/.wasmedge/lib/wasmedge_quickjs.wasm" ]; then
    WASMEDGE_QJS="$HOME/.wasmedge/lib/wasmedge_quickjs.wasm"
else
    echo "Installing wasmedge-quickjs..."
    curl -sSf https://raw.githubusercontent.com/aspect-build/aspect-cli/main/wasmedge-quickjs/install.sh | bash 2>/dev/null || true
    if [ -f "$HOME/.wasmedge/lib/wasmedge_quickjs_aot.wasm" ]; then
        WASMEDGE_QJS="$HOME/.wasmedge/lib/wasmedge_quickjs_aot.wasm"
    elif [ -f "$HOME/.wasmedge/lib/wasmedge_quickjs.wasm" ]; then
        WASMEDGE_QJS="$HOME/.wasmedge/lib/wasmedge_quickjs.wasm"
    fi
fi

# Setup porffor (fix symlink issue)
if ! npm list -g porffor &> /dev/null; then
    echo "Installing porffor..."
    npm install -g porffor 2>/dev/null || true
fi
# Fix broken porffor symlink (npm creates bad symlink)
if ! porffor -e "1" &> /dev/null; then
    PORFFOR_DIR="$(npm root -g)/porffor"
    cat > /usr/local/bin/porffor << EOF
#!/bin/sh
node "$PORFFOR_DIR/runtime/index.js" "\$@"
EOF
    chmod +x /usr/local/bin/porffor
fi

echo "═══════════════════════════════════════════════════════════════"
echo "                    EdgeBox Benchmarks"
echo "═══════════════════════════════════════════════════════════════"
echo ""
echo "Runtimes: EdgeBox, Bun, wasmedge-qjs, Node.js, Porffor"
echo ""

# Build each benchmark as static WASM
for bench in hello alloc_stress fib; do
    if [ -f "$SCRIPT_DIR/${bench}.js" ]; then
        echo "Building $bench benchmark..."
        mkdir -p "$SCRIPT_DIR/build_$bench"
        cp "$SCRIPT_DIR/${bench}.js" "$SCRIPT_DIR/build_$bench/index.js"
        cd "$ROOT_DIR"
        "$EDGEBOX" build --force "$SCRIPT_DIR/build_$bench" > /dev/null 2>&1
        mv edgebox-static.wasm "$SCRIPT_DIR/${bench}.wasm" 2>/dev/null || true
        mv edgebox-static-aot.dylib "$SCRIPT_DIR/${bench}.dylib" 2>/dev/null || true
        mv edgebox-static-aot.so "$SCRIPT_DIR/${bench}.so" 2>/dev/null || true
        rm -rf "$SCRIPT_DIR/build_$bench" bundle.js bundle_compiled.c 2>/dev/null || true
    fi
done
echo ""

# Cold Start Benchmark
echo ">>> Cold Start (hello.js)"
hyperfine --warmup 1 --runs 5 \
    --export-markdown "$SCRIPT_DIR/results_cold_start.md" \
    "$EDGEBOX $SCRIPT_DIR/hello.wasm" \
    "bun $SCRIPT_DIR/hello.js" \
    "wasmedge --dir $SCRIPT_DIR $WASMEDGE_QJS $SCRIPT_DIR/hello.js" \
    "node $SCRIPT_DIR/hello.js" \
    "porffor $SCRIPT_DIR/hello.js"
echo ""

# Allocator Stress Benchmark
echo ">>> Allocator Stress (alloc_stress.js)"
hyperfine --warmup 1 --runs 3 \
    --export-markdown "$SCRIPT_DIR/results_alloc.md" \
    "$EDGEBOX $SCRIPT_DIR/alloc_stress.wasm" \
    "bun $SCRIPT_DIR/alloc_stress.js" \
    "wasmedge --dir $SCRIPT_DIR $WASMEDGE_QJS $SCRIPT_DIR/alloc_stress.js" \
    "node $SCRIPT_DIR/alloc_stress.js" \
    "porffor $SCRIPT_DIR/alloc_stress.js"
echo ""

# Fibonacci Benchmark
echo ">>> Fibonacci(35) x100 (fib.js)"
hyperfine --warmup 0 --runs 1 \
    --export-markdown "$SCRIPT_DIR/results_fib.md" \
    "$EDGEBOX $SCRIPT_DIR/fib.wasm" \
    "bun $SCRIPT_DIR/fib.js" \
    "wasmedge --dir $SCRIPT_DIR $WASMEDGE_QJS $SCRIPT_DIR/fib.js" \
    "node $SCRIPT_DIR/fib.js" \
    "porffor $SCRIPT_DIR/fib.js"
echo ""

echo "═══════════════════════════════════════════════════════════════"
echo "Results exported to bench/results_*.md"
