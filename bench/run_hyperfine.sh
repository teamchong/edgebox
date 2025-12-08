#!/bin/bash
# EdgeBox Benchmark Suite using hyperfine
# https://github.com/sharkdp/hyperfine
#
# All WASM runtimes use WasmEdge with AOT compilation for fair comparison

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"

# Porffor path
PORFFOR="$HOME/.local/share/mise/installs/node/20.18.0/lib/node_modules/porffor/porf"
if [ ! -x "$PORFFOR" ]; then
    PORFFOR="$(npm root -g 2>/dev/null)/porffor/porf"
fi

# wasmedge-quickjs paths
WASMEDGE_QJS_AOT="$HOME/.wasmedge/lib/wasmedge_quickjs_aot.wasm"
WASMEDGE_QJS="$HOME/.wasmedge/lib/wasmedge_quickjs.wasm"

# Check for hyperfine
if ! command -v hyperfine &> /dev/null; then
    echo "hyperfine not found. Install with: brew install hyperfine"
    exit 1
fi

# Build EdgeBox first
echo "Building EdgeBox..."
cd "$ROOT_DIR"
./build.sh > /dev/null 2>&1 || { echo "Build failed!"; exit 1; }
echo ""

echo "═══════════════════════════════════════════════════════════════"
echo "                    EdgeBox Benchmarks (hyperfine)"
echo "═══════════════════════════════════════════════════════════════"
echo ""
echo "All WASM runtimes use WasmEdge with AOT compilation"
echo ""

# Determine EdgeBox AOT binary
case "$(uname -s)" in
    Darwin) EDGEBOX_AOT="$ROOT_DIR/edgebox-aot.dylib" ;;
    *)      EDGEBOX_AOT="$ROOT_DIR/edgebox-aot.so" ;;
esac

# Build benchmark command list
BENCH_CMDS=()
# EdgeBox - direct wasmedge call (no shell overhead)
if [ -f "$EDGEBOX_AOT" ]; then
    BENCH_CMDS+=("wasmedge --dir $SCRIPT_DIR $EDGEBOX_AOT $SCRIPT_DIR/hello.js")
else
    BENCH_CMDS+=("wasmedge --dir $SCRIPT_DIR $ROOT_DIR/edgebox-base.wasm $SCRIPT_DIR/hello.js")
fi

# wasmedge-quickjs (prefer AOT)
if [ -f "$WASMEDGE_QJS_AOT" ]; then
    BENCH_CMDS+=("wasmedge --dir $SCRIPT_DIR $WASMEDGE_QJS_AOT $SCRIPT_DIR/hello.js")
elif [ -f "$WASMEDGE_QJS" ]; then
    BENCH_CMDS+=("wasmedge --dir $SCRIPT_DIR $WASMEDGE_QJS $SCRIPT_DIR/hello.js")
fi

# Porffor
if [ -x "$PORFFOR" ]; then
    BENCH_CMDS+=("$PORFFOR $SCRIPT_DIR/hello.js")
fi

# Native runtimes
BENCH_CMDS+=("node $SCRIPT_DIR/hello.js")
BENCH_CMDS+=("bun $SCRIPT_DIR/hello.js")

# Cold Start Benchmark
echo ">>> Cold Start (hello.js)"
echo ""

hyperfine --warmup 3 --runs 20 \
    --export-markdown "$SCRIPT_DIR/results_cold_start.md" \
    "${BENCH_CMDS[@]}"

echo ""

# Build alloc benchmark commands (Porffor may not support all features)
ALLOC_CMDS=()
if [ -f "$EDGEBOX_AOT" ]; then
    ALLOC_CMDS+=("wasmedge --dir $SCRIPT_DIR $EDGEBOX_AOT $SCRIPT_DIR/alloc_stress.js")
else
    ALLOC_CMDS+=("wasmedge --dir $SCRIPT_DIR $ROOT_DIR/edgebox-base.wasm $SCRIPT_DIR/alloc_stress.js")
fi
if [ -f "$WASMEDGE_QJS_AOT" ]; then
    ALLOC_CMDS+=("wasmedge --dir $SCRIPT_DIR $WASMEDGE_QJS_AOT $SCRIPT_DIR/alloc_stress.js")
elif [ -f "$WASMEDGE_QJS" ]; then
    ALLOC_CMDS+=("wasmedge --dir $SCRIPT_DIR $WASMEDGE_QJS $SCRIPT_DIR/alloc_stress.js")
fi
ALLOC_CMDS+=("node $SCRIPT_DIR/alloc_stress.js")
ALLOC_CMDS+=("bun $SCRIPT_DIR/alloc_stress.js")

# Allocator Stress Benchmark
echo ">>> Allocator Stress (alloc_stress.js)"
echo ""

hyperfine --warmup 2 --runs 10 -i \
    --export-markdown "$SCRIPT_DIR/results_alloc.md" \
    "${ALLOC_CMDS[@]}"

echo ""

# Build fib benchmark commands
FIB_CMDS=()
if [ -f "$EDGEBOX_AOT" ]; then
    FIB_CMDS+=("wasmedge --dir $SCRIPT_DIR $EDGEBOX_AOT $SCRIPT_DIR/fib.js")
else
    FIB_CMDS+=("wasmedge --dir $SCRIPT_DIR $ROOT_DIR/edgebox-base.wasm $SCRIPT_DIR/fib.js")
fi
if [ -f "$WASMEDGE_QJS_AOT" ]; then
    FIB_CMDS+=("wasmedge --dir $SCRIPT_DIR $WASMEDGE_QJS_AOT $SCRIPT_DIR/fib.js")
elif [ -f "$WASMEDGE_QJS" ]; then
    FIB_CMDS+=("wasmedge --dir $SCRIPT_DIR $WASMEDGE_QJS $SCRIPT_DIR/fib.js")
fi
if [ -x "$PORFFOR" ]; then
    FIB_CMDS+=("$PORFFOR $SCRIPT_DIR/fib.js")
fi
FIB_CMDS+=("node $SCRIPT_DIR/fib.js")
FIB_CMDS+=("bun $SCRIPT_DIR/fib.js")

# Fibonacci Benchmark (CPU-intensive)
echo ">>> Fibonacci(35) x100 iterations (fib.js)"
echo ""

hyperfine --warmup 1 --runs 5 \
    --export-markdown "$SCRIPT_DIR/results_fib.md" \
    "${FIB_CMDS[@]}"

echo ""
echo "═══════════════════════════════════════════════════════════════"
echo "Results exported to:"
echo "  - bench/results_cold_start.md"
echo "  - bench/results_alloc.md"
echo "  - bench/results_fib.md"
