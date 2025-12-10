#!/bin/bash
# EdgeBox Full Benchmark Suite
# Compares 6 runtimes (EdgeBox, EdgeBox daemon, Bun, wasmedge-qjs, Node.js, Porffor)
# Across 3 benchmarks (Cold Start, Alloc Stress, CPU fib)

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
EDGEBOX="$ROOT_DIR/zig-out/bin/edgebox"
EDGEBOXC="$ROOT_DIR/zig-out/bin/edgeboxc"

# Check for hyperfine
if ! command -v hyperfine &> /dev/null; then
    echo "Installing hyperfine..."
    brew install hyperfine
fi

# Build edgebox CLI if needed
if [ ! -x "$EDGEBOX" ] || [ ! -x "$EDGEBOXC" ]; then
    echo "Building edgebox CLI..."
    cd "$ROOT_DIR" && zig build cli -Doptimize=ReleaseFast
fi

# Build benchmark WASM files if needed
build_bench_wasm() {
    local name=$1
    local js_file="$SCRIPT_DIR/$name.js"
    local dylib_file="$SCRIPT_DIR/$name.dylib"

    # Check if dylib needs rebuild (missing, older than js, older than edgeboxc, or version mismatch)
    local needs_rebuild=false
    if [ ! -f "$dylib_file" ]; then
        needs_rebuild=true
    elif [ "$js_file" -nt "$dylib_file" ]; then
        needs_rebuild=true
    elif [ "$EDGEBOXC" -nt "$dylib_file" ]; then
        needs_rebuild=true
    elif timeout 2 "$EDGEBOX" "$dylib_file" 2>&1 | grep -q "Mismatched version"; then
        needs_rebuild=true
    elif ! timeout 2 "$EDGEBOX" "$dylib_file" >/dev/null 2>&1; then
        # If dylib times out or fails, rebuild it
        needs_rebuild=true
    fi

    if $needs_rebuild; then
        echo "Building $name.dylib..."
        rm -f "$dylib_file"
        mkdir -p "$SCRIPT_DIR/build_$name"
        cp "$js_file" "$SCRIPT_DIR/build_$name/index.js"
        cd "$ROOT_DIR"
        "$EDGEBOXC" build "$SCRIPT_DIR/build_$name" 2>/dev/null || true
        if [ -f "edgebox-static-aot.dylib" ]; then
            mv edgebox-static-aot.dylib "$dylib_file"
            rm -f edgebox-static.wasm bundle.js bundle_compiled.c 2>/dev/null
        fi
    fi
}

# Build all benchmark WASM files
build_bench_wasm hello
build_bench_wasm alloc_stress
build_bench_wasm fib

# Setup wasmedge-quickjs (download if needed)
WASMEDGE_QJS="$HOME/.wasmedge/lib/wasmedge_quickjs.wasm"
WASMEDGE_QJS_AOT="$HOME/.wasmedge/lib/wasmedge_quickjs_aot.wasm"

if [ ! -f "$WASMEDGE_QJS" ] && [ ! -f "$WASMEDGE_QJS_AOT" ]; then
    echo "Downloading wasmedge-quickjs from second-state..."
    mkdir -p "$HOME/.wasmedge/lib"
    curl -L -o "$WASMEDGE_QJS" "https://github.com/second-state/wasmedge-quickjs/releases/download/v0.6.1-alpha/wasmedge_quickjs.wasm" 2>/dev/null || \
    curl -L -o "$WASMEDGE_QJS" "https://github.com/second-state/wasmedge-quickjs/releases/download/v0.5.0-alpha/wasmedge_quickjs.wasm" 2>/dev/null || \
    echo "Warning: Could not download wasmedge-quickjs, skipping"
fi

# Validate download (should be > 1MB)
if [ -f "$WASMEDGE_QJS" ]; then
    size=$(wc -c < "$WASMEDGE_QJS")
    if [ "$size" -lt 1000000 ]; then
        echo "Warning: Downloaded wasmedge-quickjs is too small ($size bytes), removing"
        rm -f "$WASMEDGE_QJS"
    fi
fi

# Use AOT version if available
if [ -f "$WASMEDGE_QJS_AOT" ]; then
    WASMEDGE_QJS="$WASMEDGE_QJS_AOT"
fi

# Porffor path
PORFFOR=""
if [ -x "$HOME/.local/share/mise/installs/node/20.18.0/lib/node_modules/porffor/porf" ]; then
    PORFFOR="$HOME/.local/share/mise/installs/node/20.18.0/lib/node_modules/porffor/porf"
elif command -v porf &> /dev/null; then
    PORFFOR="porf"
fi

# Build Porffor native binaries if Porffor is available
build_porffor_native() {
    local name=$1
    local js_file="$SCRIPT_DIR/$name.js"
    local native_file="$SCRIPT_DIR/${name}_porffor"

    if [ -n "$PORFFOR" ]; then
        if [ ! -f "$native_file" ] || [ "$js_file" -nt "$native_file" ]; then
            echo "Building ${name}_porffor (native)..."
            "$PORFFOR" native "$js_file" "$native_file" 2>/dev/null || true
        fi
    fi
}

if [ -n "$PORFFOR" ]; then
    build_porffor_native hello
    build_porffor_native alloc_stress
    build_porffor_native fib
fi

echo ""
echo "═══════════════════════════════════════════════════════════════"
echo "                    EdgeBox Benchmark Suite"
echo "═══════════════════════════════════════════════════════════════"
echo ""
echo "Runtimes: EdgeBox (WASM), EdgeBox (daemon), Bun (CLI), Node.js (CLI)"
[ -f "$WASMEDGE_QJS" ] && echo "          wasmedge-qjs (WASM)"
[ -n "$PORFFOR" ] && echo "          Porffor (WASM), Porffor (CLI)"
echo ""

# edgeboxd for daemon mode benchmarks
EDGEBOXD="$ROOT_DIR/zig-out/bin/edgeboxd"
DAEMON_PORT=18080
DAEMON_PID=""

# Start daemon for benchmarks
start_daemon() {
    local dylib_file=$1
    if [ -x "$EDGEBOXD" ] && [ -f "$dylib_file" ]; then
        "$EDGEBOXD" "$dylib_file" --port=$DAEMON_PORT >/dev/null 2>&1 &
        DAEMON_PID=$!
        sleep 0.5
        # Verify it started (use nc for faster check)
        if printf "GET / HTTP/1.0\r\n\r\n" | nc -w1 localhost $DAEMON_PORT >/dev/null 2>&1; then
            return 0
        fi
    fi
    return 1
}

# Stop daemon
stop_daemon() {
    if [ -n "$DAEMON_PID" ]; then
        kill $DAEMON_PID 2>/dev/null || true
        DAEMON_PID=""
        sleep 0.2
    fi
}

# Build hyperfine command dynamically based on available runtimes
run_benchmark() {
    local name=$1
    local runs=$2
    local warmup=$3
    local edgebox_file=$4
    local js_file=$5
    local output_file=$6

    # Start daemon for this benchmark
    start_daemon "$edgebox_file"
    local daemon_available=$?

    local porffor_native="$SCRIPT_DIR/${name}_porffor"

    # Use timeout wrapper to report TIMEOUT instead of silently missing
    # 300s (5 min) timeout per command for fib(35) which takes ~1.5s * runs
    local timeout_cmd="timeout 300"

    local cmd="hyperfine --warmup $warmup --runs $runs"
    cmd+=" -n 'EdgeBox (WASM)' '$timeout_cmd $EDGEBOX $edgebox_file 2>/dev/null || echo TIMEOUT'"
    [ $daemon_available -eq 0 ] && cmd+=" -n 'EdgeBox (daemon)' '$timeout_cmd curl -s http://localhost:$DAEMON_PORT/ || echo TIMEOUT'"
    cmd+=" -n 'Bun (CLI)' '$timeout_cmd bun $js_file || echo TIMEOUT'"
    [ -f "$WASMEDGE_QJS" ] && cmd+=" -n 'wasmedge-qjs (WASM)' '$timeout_cmd wasmedge --dir $SCRIPT_DIR $WASMEDGE_QJS $js_file || echo TIMEOUT'"
    cmd+=" -n 'Node.js (CLI)' '$timeout_cmd node $js_file || echo TIMEOUT'"
    [ -n "$PORFFOR" ] && cmd+=" -n 'Porffor (WASM)' '$timeout_cmd $PORFFOR $js_file || echo TIMEOUT'"
    [ -x "$porffor_native" ] && cmd+=" -n 'Porffor (CLI)' '$timeout_cmd $porffor_native || echo TIMEOUT'"
    cmd+=" --export-markdown '$output_file'"

    eval $cmd || echo "WARNING: hyperfine failed for $name benchmark"

    # Stop daemon after benchmark
    stop_daemon
}

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 1: Cold Start (hello.js)
# ─────────────────────────────────────────────────────────────────
echo "─────────────────────────────────────────────────────────────────"
echo "1. Cold Start (hello.js)"
echo "─────────────────────────────────────────────────────────────────"

run_benchmark "hello" 20 3 "$SCRIPT_DIR/hello.dylib" "$SCRIPT_DIR/hello.js" "$SCRIPT_DIR/results_cold_start.md"

echo ""

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 2: Alloc Stress (30k allocations)
# ─────────────────────────────────────────────────────────────────
echo "─────────────────────────────────────────────────────────────────"
echo "2. Alloc Stress (30k allocations)"
echo "─────────────────────────────────────────────────────────────────"

run_benchmark "alloc_stress" 10 3 "$SCRIPT_DIR/alloc_stress.dylib" "$SCRIPT_DIR/alloc_stress.js" "$SCRIPT_DIR/results_alloc.md"

echo ""

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 3: CPU fib(35)
# ─────────────────────────────────────────────────────────────────
echo "─────────────────────────────────────────────────────────────────"
echo "3. CPU fib(35)"
echo "─────────────────────────────────────────────────────────────────"

run_benchmark "fib" 3 1 "$SCRIPT_DIR/fib.dylib" "$SCRIPT_DIR/fib.js" "$SCRIPT_DIR/results_fib.md"

echo ""
echo "═══════════════════════════════════════════════════════════════"
echo "                    Benchmark Complete!"
echo "═══════════════════════════════════════════════════════════════"
echo ""
echo "Results saved to:"
echo "  - $SCRIPT_DIR/results_cold_start.md"
echo "  - $SCRIPT_DIR/results_alloc.md"
echo "  - $SCRIPT_DIR/results_fib.md"
