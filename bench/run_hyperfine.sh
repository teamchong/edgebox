#!/bin/bash
# EdgeBox Full Benchmark Suite
# Compares 5 runtimes (EdgeBox, EdgeBox daemon, Bun, Node.js, Porffor)
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

# WAMR AOT compiler
WAMRC="$ROOT_DIR/vendor/wamr/wamr-compiler/build/wamrc"

# Build benchmark AOT files for fast cold start
build_bench_aot() {
    local name=$1
    local wasm_file="$SCRIPT_DIR/$name.wasm"
    local aot_file="$SCRIPT_DIR/$name.aot"

    # Check if AOT needs rebuild
    if [ -f "$wasm_file" ] && [ -x "$WAMRC" ]; then
        if [ ! -f "$aot_file" ] || [ "$wasm_file" -nt "$aot_file" ]; then
            echo "AOT compiling $name.aot..."
            "$WAMRC" -o "$aot_file" "$wasm_file" 2>/dev/null || true
        fi
    fi
}

# Build all benchmark AOT files
build_bench_aot hello
build_bench_aot alloc_stress
build_bench_aot fib

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

    # Daemon disabled - edgeboxd still uses WasmEdge, needs migration to WAMR
    # start_daemon "$edgebox_file"
    local daemon_available=1  # 1 = not available

    local porffor_native="$SCRIPT_DIR/${name}_porffor"

    # Use timeout wrapper to report TIMEOUT instead of silently missing
    # 60s (1 min) timeout for slow benchmarks
    local timeout_cmd="timeout 60"

    local cmd="hyperfine --warmup $warmup --runs $runs"
    cmd+=" -n 'EdgeBox (WASM)' '$timeout_cmd $EDGEBOX $edgebox_file 2>/dev/null || echo TIMEOUT'"
    [ $daemon_available -eq 0 ] && cmd+=" -n 'EdgeBox (daemon)' '$timeout_cmd curl -s http://localhost:$DAEMON_PORT/ || echo TIMEOUT'"
    cmd+=" -n 'Bun (CLI)' '$timeout_cmd bun $js_file || echo TIMEOUT'"
    cmd+=" -n 'Node.js (CLI)' '$timeout_cmd node $js_file || echo TIMEOUT'"
    [ -n "$PORFFOR" ] && cmd+=" -n 'Porffor (WASM)' '$timeout_cmd $PORFFOR $js_file || echo TIMEOUT'"
    [ -x "$porffor_native" ] && cmd+=" -n 'Porffor (CLI)' '$timeout_cmd $porffor_native || echo TIMEOUT'"
    cmd+=" --export-markdown '$output_file'"

    eval $cmd || echo "WARNING: hyperfine failed for $name benchmark"

    # Daemon disabled
    # stop_daemon
}

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 1: Cold Start (hello.js)
# ─────────────────────────────────────────────────────────────────
echo "─────────────────────────────────────────────────────────────────"
echo "1. Cold Start (hello.js)"
echo "─────────────────────────────────────────────────────────────────"

run_benchmark "hello" 20 3 "$SCRIPT_DIR/hello.aot" "$SCRIPT_DIR/hello.js" "$SCRIPT_DIR/results_cold_start.md"

echo ""

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 2: Alloc Stress (30k allocations)
# ─────────────────────────────────────────────────────────────────
echo "─────────────────────────────────────────────────────────────────"
echo "2. Alloc Stress (30k allocations)"
echo "─────────────────────────────────────────────────────────────────"

run_benchmark "alloc_stress" 10 3 "$SCRIPT_DIR/alloc_stress.aot" "$SCRIPT_DIR/alloc_stress.js" "$SCRIPT_DIR/results_alloc.md"

echo ""

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 3: CPU fib(35)
# ─────────────────────────────────────────────────────────────────
echo "─────────────────────────────────────────────────────────────────"
echo "3. CPU fib(35)"
echo "─────────────────────────────────────────────────────────────────"

run_benchmark "fib" 3 1 "$SCRIPT_DIR/fib.aot" "$SCRIPT_DIR/fib.js" "$SCRIPT_DIR/results_fib.md"

echo ""
echo "═══════════════════════════════════════════════════════════════"
echo "                    Benchmark Complete!"
echo "═══════════════════════════════════════════════════════════════"
echo ""
echo "Results saved to:"
echo "  - $SCRIPT_DIR/results_cold_start.md"
echo "  - $SCRIPT_DIR/results_alloc.md"
echo "  - $SCRIPT_DIR/results_fib.md"
