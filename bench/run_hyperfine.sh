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

# Build benchmark: JS -> AOT (edgeboxc does everything)
build_bench() {
    local name=$1
    local js_file="$SCRIPT_DIR/$name.js"
    local aot_file="$SCRIPT_DIR/$name.aot"

    # edgeboxc build produces edgebox-static.aot directly (includes frozen functions + AOT compile)
    if [ -f "$js_file" ]; then
        if [ ! -f "$aot_file" ] || [ "$js_file" -nt "$aot_file" ]; then
            echo "Building $name.aot with edgeboxc (frozen functions + AOT)..."
            cd "$ROOT_DIR" && "$EDGEBOXC" build "$js_file" 2>&1 | grep -v "^\[" || true
            # edgeboxc outputs to edgebox-static.aot in cwd, move to bench/
            if [ -f "$ROOT_DIR/edgebox-static.aot" ]; then
                mv "$ROOT_DIR/edgebox-static.aot" "$aot_file"
            fi
            # Clean up build artifacts
            rm -f "$ROOT_DIR/bundle.js" "$ROOT_DIR/bundle_compiled.c" "$ROOT_DIR/frozen_functions.c" \
                  "$ROOT_DIR/frozen_manifest.json" "$ROOT_DIR/edgebox-static.wasm" 2>/dev/null
        fi
    fi
}

# Build all benchmarks
build_bench hello
build_bench alloc_stress
build_bench fib

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
    # Build Porffor WASM for fib benchmark
    if [ ! -f "$SCRIPT_DIR/fib_porf.wasm" ] || [ "$SCRIPT_DIR/fib.js" -nt "$SCRIPT_DIR/fib_porf.wasm" ]; then
        echo "Building fib_porf.wasm..."
        "$PORFFOR" wasm "$SCRIPT_DIR/fib.js" "$SCRIPT_DIR/fib_porf.wasm" 2>/dev/null || true
    fi
fi

echo ""
echo "═══════════════════════════════════════════════════════════════"
echo "                    EdgeBox Benchmark Suite"
echo "═══════════════════════════════════════════════════════════════"
echo ""
echo "Runtimes: EdgeBox (WASM), EdgeBox (daemon), Bun (CLI), Node.js (CLI)"
[ -n "$PORFFOR" ] && echo "          Porffor (Node+V8), Porffor (Native)"
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

    # Start daemon for this benchmark (now using WAMR)
    start_daemon "$edgebox_file"
    local daemon_available=$?

    local porffor_native="$SCRIPT_DIR/${name}_porffor"

    # Use timeout wrapper to report TIMEOUT instead of silently missing
    # 60s (1 min) timeout for slow benchmarks
    local timeout_cmd="timeout 60"

    local cmd="hyperfine --warmup $warmup --runs $runs"
    cmd+=" -n 'EdgeBox (WASM)' '$timeout_cmd $EDGEBOX $edgebox_file 2>/dev/null || echo TIMEOUT'"
    [ $daemon_available -eq 0 ] && cmd+=" -n 'EdgeBox (daemon)' '$timeout_cmd curl -s http://localhost:$DAEMON_PORT/ || echo TIMEOUT'"
    cmd+=" -n 'Bun (CLI)' '$timeout_cmd bun $js_file || echo TIMEOUT'"
    cmd+=" -n 'Node.js (CLI)' '$timeout_cmd node $js_file || echo TIMEOUT'"
    [ -x "$porffor_native" ] && cmd+=" -n 'Porffor (Native)' '$timeout_cmd $porffor_native || echo TIMEOUT'"
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
# BENCHMARK 3: CPU fib(40) - with result validation
# ─────────────────────────────────────────────────────────────────
echo "─────────────────────────────────────────────────────────────────"
echo "3. CPU fib(45) - Frozen Interpreter Benchmark"
echo "─────────────────────────────────────────────────────────────────"

# Validate all runtimes produce correct result before benchmarking
EXPECTED="1134903170"
echo "Validating results (expected fib(45) = $EXPECTED)..."

validate_fib() {
    local name=$1
    local cmd=$2
    local output=$(eval "$cmd" 2>/dev/null | tail -1)
    local result=$(echo "$output" | grep -oE '^[0-9]{10}' | head -1)
    local time=$(echo "$output" | grep -oE '\([0-9.]+ms\)' | head -1)
    if [ "$result" = "$EXPECTED" ]; then
        echo "  ✓ $name: $result $time"
        return 0
    else
        echo "  ✗ $name: got '$result' (INVALID)"
        return 1
    fi
}

validate_fib "EdgeBox AOT" "$EDGEBOX $SCRIPT_DIR/fib.aot"
validate_fib "Bun" "bun $SCRIPT_DIR/fib.js"
validate_fib "Node.js" "node $SCRIPT_DIR/fib.js"
[ -n "$PORFFOR" ] && validate_fib "Porffor" "$PORFFOR $SCRIPT_DIR/fib.js"

echo ""
echo "Running benchmark (using performance.now() for pure computation time)..."

# Collect timing from performance.now() output - this measures pure runtime, not startup
FIB_PORFFOR="$SCRIPT_DIR/fib_porffor"
declare -A TIMES

run_fib() {
    local name=$1
    local cmd=$2
    local output=$(eval "$cmd" 2>/dev/null | tail -1)
    local time=$(echo "$output" | grep -oE '\([0-9.]+ms\)' | grep -oE '[0-9.]+')
    if [ -n "$time" ]; then
        echo "  $name: ${time}ms"
        TIMES["$name"]=$time
    else
        echo "  $name: FAILED"
    fi
}

echo ""
run_fib "EdgeBox (AOT)" "$EDGEBOX $SCRIPT_DIR/fib.aot"
run_fib "Bun" "bun $SCRIPT_DIR/fib.js"
run_fib "Node.js" "node $SCRIPT_DIR/fib.js"
[ -f "$SCRIPT_DIR/fib_porf.wasm" ] && run_fib "Porffor (WASM)" "node $SCRIPT_DIR/run_porf_wasm.js"
[ -x "$FIB_PORFFOR" ] && run_fib "Porffor (Native)" "$FIB_PORFFOR"

# Generate markdown results
echo ""
echo "Generating results_fib.md..."
cat > "$SCRIPT_DIR/results_fib.md" << 'HEADER'
| Runtime | Computation Time | Relative |
|:---|---:|---:|
HEADER

# Calculate relative times (EdgeBox as baseline)
BASELINE=${TIMES["EdgeBox (AOT)"]}
if [ -n "$BASELINE" ]; then
    for name in "EdgeBox (AOT)" "Bun" "Node.js" "Porffor (WASM)" "Porffor (Native)"; do
        time=${TIMES["$name"]}
        if [ -n "$time" ]; then
            # Use bc for floating point division
            relative=$(echo "scale=2; $time / $BASELINE" | bc)
            if [ "$name" = "EdgeBox (AOT)" ]; then
                echo "| \`$name\` | ${time}ms | **1.00** |" >> "$SCRIPT_DIR/results_fib.md"
            else
                echo "| \`$name\` | ${time}ms | ${relative}x |" >> "$SCRIPT_DIR/results_fib.md"
            fi
        fi
    done
fi

cat "$SCRIPT_DIR/results_fib.md"

echo ""

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 4: Daemon Warm Pod (pre-allocated instances)
# ─────────────────────────────────────────────────────────────────
echo "─────────────────────────────────────────────────────────────────"
echo "4. Daemon Warm Pod (pre-allocated batch pool)"
echo "─────────────────────────────────────────────────────────────────"

# Start daemon with batch pool (pre-allocated instances)
if [ -x "$EDGEBOXD" ]; then
    "$EDGEBOXD" "$SCRIPT_DIR/hello.aot" --pool-size=32 --port=$DAEMON_PORT >/dev/null 2>&1 &
    DAEMON_PID=$!
    sleep 2  # Wait for pool to fill

    if printf "GET / HTTP/1.0\r\n\r\n" | nc -w1 localhost $DAEMON_PORT >/dev/null 2>&1; then
        echo "Daemon started with 32 pre-allocated instances"
        echo ""

        # Warm up the pool (first request may be slightly slower)
        curl -s http://localhost:$DAEMON_PORT/ >/dev/null 2>&1

        # Benchmark warm pod latency
        hyperfine --warmup 5 --runs 50 \
            -n 'EdgeBox (daemon warm)' "curl -s http://localhost:$DAEMON_PORT/" \
            --export-markdown "$SCRIPT_DIR/results_daemon_warm.md"

        stop_daemon
        echo ""
        echo "Daemon warm pod benchmark complete!"
    else
        echo "WARNING: Could not start daemon for warm pod benchmark"
    fi
else
    echo "SKIP: edgeboxd not found"
fi

echo ""
echo "═══════════════════════════════════════════════════════════════"
echo "                    Benchmark Complete!"
echo "═══════════════════════════════════════════════════════════════"
echo ""
echo "Results saved to:"
echo "  - $SCRIPT_DIR/results_cold_start.md"
echo "  - $SCRIPT_DIR/results_alloc.md"
echo "  - $SCRIPT_DIR/results_fib.md"
echo "  - $SCRIPT_DIR/results_daemon_warm.md"
