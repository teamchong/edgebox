#!/bin/bash
# EdgeBox Full Benchmark Suite
# Compares runtimes: EdgeBox (AOT, WASM, daemon), Bun, Node.js, Porffor
# Across benchmarks: Cold Start, Memory Usage, CPU fib, Daemon Warm Pod

set -e

# CI mode: fewer runs, skip slow benchmarks
CI_MODE=false
if [ "$1" = "--ci" ]; then
    CI_MODE=true
    echo "Running in CI mode (fewer runs, skip daemon benchmark)"
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"

# Verify we can find the repo
if [ ! -f "$ROOT_DIR/build.zig" ]; then
    echo "ERROR: Cannot find EdgeBox repo. Expected build.zig at: $ROOT_DIR/build.zig"
    echo ""
    echo "Run this script from within the EdgeBox repo:"
    echo "  cd /path/to/edgebox && ./bench/run_hyperfine.sh"
    exit 1
fi

EDGEBOX="$ROOT_DIR/zig-out/bin/edgebox"
EDGEBOXC="$ROOT_DIR/zig-out/bin/edgeboxc"

# edgebox-rosetta for Fast JIT under Rosetta 2 (macOS ARM64 only)
EDGEBOX_ROSETTA="$ROOT_DIR/zig-out/bin/edgebox-rosetta"

# Check for hyperfine
if ! command -v hyperfine &> /dev/null; then
    echo "Installing hyperfine..."
    brew install hyperfine
fi

# Detect platform - Linux x86_64 can use Fast JIT, Mac ARM64 uses native Fast Interpreter
PLATFORM=$(uname -s)
ARCH=$(uname -m)
WASM_RUNNER="$EDGEBOX"  # Default to native edgebox

if [ "$PLATFORM" = "Linux" ] && [ "$ARCH" = "x86_64" ]; then
    echo "Platform: Linux x86_64 - using native Fast JIT"
    # On Linux x86_64, edgebox is built with Fast JIT
    # Prerequisites: WAMR must be built with Fast JIT in vendor/wamr/product-mini/platforms/linux/build/
    if [ ! -f "$ROOT_DIR/vendor/wamr/product-mini/platforms/linux/build/libiwasm.a" ]; then
        echo "WARNING: WAMR library not found. Building with Fast JIT..."
        echo "  cd vendor/wamr/product-mini/platforms/linux && mkdir -p build && cd build"
        echo "  cmake .. -DWAMR_BUILD_FAST_JIT=1 -DWAMR_BUILD_INTERP=1 -DWAMR_BUILD_AOT=1 -DWAMR_BUILD_LIBC_WASI=1 -DWAMR_BUILD_SIMD=0 -DCMAKE_BUILD_TYPE=Release"
        echo "  make -j\$(nproc)"
        echo ""
        (cd "$ROOT_DIR/vendor/wamr/product-mini/platforms/linux" && \
         mkdir -p build && cd build && \
         cmake .. -DWAMR_BUILD_FAST_JIT=1 -DWAMR_BUILD_INTERP=1 -DWAMR_BUILD_AOT=1 -DWAMR_BUILD_LIBC_WASI=1 -DWAMR_BUILD_SIMD=0 -DCMAKE_BUILD_TYPE=Release && \
         make -j$(nproc)) || {
            echo "ERROR: Failed to build WAMR. Please build manually."
            exit 1
        }
    fi
elif [ "$PLATFORM" = "Darwin" ]; then
    if [ "$ARCH" = "arm64" ]; then
        echo "Platform: macOS ARM64 - using edgebox-rosetta (Fast JIT via Rosetta 2)"

        # edgebox-rosetta is x86_64 binary that runs via Rosetta 2 with Fast JIT
        # This is faster than native ARM64 interpreter for WASM execution
        if [ -x "$EDGEBOX_ROSETTA" ]; then
            WASM_RUNNER="$EDGEBOX_ROSETTA"
        else
            echo "WARNING: edgebox-rosetta not found, building..."
            (cd "$ROOT_DIR" && zig build runner-rosetta -Doptimize=ReleaseFast) || {
                echo "ERROR: Failed to build edgebox-rosetta"
                exit 1
            }
            WASM_RUNNER="$EDGEBOX_ROSETTA"
        fi
    else
        echo "Platform: macOS x86_64 - using native Fast JIT"
        WASM_RUNNER="$EDGEBOX"
    fi
else
    echo "WARNING: Unknown platform $PLATFORM/$ARCH - WASM benchmarks may be slow (interpreter mode)"
fi

# Always rebuild CLI to ensure latest freeze code is used
echo "Building edgebox CLI..."
cd "$ROOT_DIR" && zig build cli -Doptimize=ReleaseFast

# Build benchmark: JS -> WASM + AOT (edgeboxc handles everything)
build_bench() {
    local name=$1
    local js_file="$SCRIPT_DIR/$name.js"
    local wasm_file="$SCRIPT_DIR/$name.wasm"
    local aot_file="$SCRIPT_DIR/$name.aot"

    # Always rebuild for benchmarks - ensures accurate results with latest code
    if [ -f "$js_file" ]; then
        echo "Building $name.wasm and $name.aot..."
        # Remove old cached files first to ensure fresh build
        rm -f "$wasm_file" "$aot_file" "$ROOT_DIR/$name.wasm" "$ROOT_DIR/$name.aot" 2>/dev/null
        cd "$ROOT_DIR" && "$EDGEBOXC" build "$js_file" 2>&1 | grep -v "^\[" | grep -v "^  Atom" || true
        # edgeboxc outputs <name>.wasm and <name>.aot to current directory
        [ -f "$ROOT_DIR/$name.wasm" ] && mv "$ROOT_DIR/$name.wasm" "$wasm_file"
        [ -f "$ROOT_DIR/$name.aot" ] && mv "$ROOT_DIR/$name.aot" "$aot_file"
        rm -f "$ROOT_DIR"/bundle*.{js,c} "$ROOT_DIR"/frozen_*.{c,json} 2>/dev/null
    fi
}

# Build all benchmarks
build_bench hello
build_bench memory
build_bench fib
build_bench tail_recursive

# Porffor path
PORFFOR=""
if [ -x "$HOME/.local/share/mise/installs/node/20.18.0/lib/node_modules/porffor/porf" ]; then
    PORFFOR="$HOME/.local/share/mise/installs/node/20.18.0/lib/node_modules/porffor/porf"
elif command -v porf &> /dev/null; then
    PORFFOR="porf"
fi

# Porffor runs JS directly via CLI (no WASM build needed)

echo ""
echo "═══════════════════════════════════════════════════════════════"
echo "                    EdgeBox Benchmark Suite"
echo "═══════════════════════════════════════════════════════════════"
echo ""
echo "Runtimes: EdgeBox (AOT), EdgeBox (WASM), EdgeBox (daemon), Bun, Node.js"
[ -n "$PORFFOR" ] && echo "          Porffor (porf <js>)"
if [ "$WASM_RUNNER" != "$EDGEBOX" ]; then
    echo ""
    echo "Note: WASM runs via native Fast Interpreter (instant startup, efficient on ARM64)"
fi
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
        wait $DAEMON_PID 2>/dev/null || true  # Suppress "Terminated" message
        DAEMON_PID=""
        sleep 0.2
    fi
}

# Set benchmark parameters based on CI mode
if [ "$CI_MODE" = true ]; then
    BENCH_RUNS=3
    BENCH_WARMUP=1
else
    BENCH_RUNS=20
    BENCH_WARMUP=3
fi

# Build hyperfine command dynamically based on available runtimes
run_benchmark() {
    local name=$1
    local runs=${2:-$BENCH_RUNS}
    local warmup=${3:-$BENCH_WARMUP}
    local aot_file=$4
    local js_file=$5
    local output_file=$6
    local wasm_file="$SCRIPT_DIR/$name.wasm"

    # Start daemon for this benchmark (now using WAMR)
    start_daemon "$aot_file"
    local daemon_available=$?

    # Use timeout wrapper to report TIMEOUT instead of silently missing
    # 60s (1 min) timeout for slow benchmarks
    local timeout_cmd="timeout 60"

    local cmd="hyperfine --warmup $warmup --runs $runs"
    cmd+=" -n 'EdgeBox (AOT)' '$timeout_cmd $EDGEBOX $aot_file 2>/dev/null || echo TIMEOUT'"
    # WASM: use WASM_RUNNER (native Fast Interpreter on ARM64)
    if [ -f "$wasm_file" ]; then
        cmd+=" -n 'EdgeBox (WASM)' '$timeout_cmd $WASM_RUNNER $wasm_file 2>/dev/null || echo TIMEOUT'"
    fi
    [ $daemon_available -eq 0 ] && cmd+=" -n 'EdgeBox (daemon)' '$timeout_cmd curl -s http://localhost:$DAEMON_PORT/ || echo TIMEOUT'"
    cmd+=" -n 'Bun (CLI)' '$timeout_cmd bun $js_file || echo TIMEOUT'"
    cmd+=" -n 'Node.js (CLI)' '$timeout_cmd node $js_file || echo TIMEOUT'"
    # Porffor (porf <js>) (runs JS directly)
    [ -n "$PORFFOR" ] && cmd+=" -n 'Porffor (porf <js>)' '$timeout_cmd $PORFFOR $js_file 2>/dev/null || echo TIMEOUT'"
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

# Display file sizes for comparison
echo ""
echo "File sizes:"
get_size() { ls -lh "$1" 2>/dev/null | awk '{print $5}' || echo "N/A"; }
echo "  EdgeBox AOT:    $(get_size $SCRIPT_DIR/hello.aot)"
echo "  EdgeBox WASM:   $(get_size $SCRIPT_DIR/hello.wasm)"
echo "  JS source:      $(get_size $SCRIPT_DIR/hello.js)"
echo ""

# Cold start = no warmup (warmup=0), measures actual first-run time
run_benchmark "hello" $BENCH_RUNS 0 "$SCRIPT_DIR/hello.aot" "$SCRIPT_DIR/hello.js" "$SCRIPT_DIR/results_cold_start.md"

echo ""

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 2: Memory Usage (600k objects)
# ─────────────────────────────────────────────────────────────────
echo "─────────────────────────────────────────────────────────────────"
echo "2. Memory Usage (600k objects - peak RSS via /usr/bin/time)"
echo "─────────────────────────────────────────────────────────────────"

get_mem() {
    local output=$(/usr/bin/time -l "$@" 2>&1)
    local bytes=$(echo "$output" | grep "maximum resident set size" | awk '{print $1}')
    echo "scale=1; $bytes / 1024 / 1024" | bc
}

echo ""
echo "  EdgeBox (AOT): $(get_mem $EDGEBOX $SCRIPT_DIR/memory.aot 2>/dev/null)MB"
[ -f "$SCRIPT_DIR/memory.wasm" ] && echo "  EdgeBox (WASM): $(get_mem $WASM_RUNNER $SCRIPT_DIR/memory.wasm 2>/dev/null)MB"
echo "  Bun: $(get_mem bun $SCRIPT_DIR/memory.js)MB"
echo "  Node.js: $(get_mem node $SCRIPT_DIR/memory.js)MB"
[ -x "$SCRIPT_DIR/memory_porffor" ] && echo "  Porffor (porf <js>): $(get_mem $SCRIPT_DIR/memory_porffor)MB"

echo ""

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 3: CPU fib(45) - with result validation
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
    # Grep for result line (10-digit number followed by timing) to handle debug output
    local output=$(eval "$cmd" 2>/dev/null | grep -E '^[0-9]{10} \(' | head -1)
    local result=$(echo "$output" | grep -oE '^[0-9]{10}' | head -1)
    local time=$(echo "$output" | grep -oE '\([0-9.]+ms' | grep -oE '[0-9.]+' | head -1)
    if [ "$result" = "$EXPECTED" ]; then
        echo "  ✓ $name: $result (${time}ms avg)"
        return 0
    else
        echo "  ✗ $name: got '$result' (INVALID)"
        return 1
    fi
}

validate_fib "EdgeBox AOT" "$EDGEBOX $SCRIPT_DIR/fib.aot"
# WASM: use WASM_RUNNER (native or x86_64 via Rosetta for Fast JIT)
if [ -f "$SCRIPT_DIR/fib.wasm" ]; then
    validate_fib "EdgeBox WASM" "$WASM_RUNNER $SCRIPT_DIR/fib.wasm"
fi
validate_fib "Bun" "bun $SCRIPT_DIR/fib.js"
validate_fib "Node.js" "node $SCRIPT_DIR/fib.js"
# Porffor (porf <js>) (runs JS directly)
[ -n "$PORFFOR" ] && validate_fib "Porffor (porf <js>)" "$PORFFOR $SCRIPT_DIR/fib.js"

echo ""
echo "Running benchmark (using performance.now() for pure computation time)..."

get_time() {
    # Grep for result line (10-digit number followed by timing) to handle debug output
    local output=$(eval "$1" 2>/dev/null | grep -E '^[0-9]{10} \(' | head -1)
    echo "$output" | grep -oE '\([0-9.]+ms' | grep -oE '[0-9.]+' | head -1
}

# Run all benchmarks and collect times
echo ""
EDGEBOX_AOT_TIME=$(get_time "$EDGEBOX $SCRIPT_DIR/fib.aot")
echo "  EdgeBox (AOT): ${EDGEBOX_AOT_TIME}ms avg"

EDGEBOX_WASM_TIME=""
if [ -f "$SCRIPT_DIR/fib.wasm" ]; then
    EDGEBOX_WASM_TIME=$(get_time "$WASM_RUNNER $SCRIPT_DIR/fib.wasm")
    echo "  EdgeBox (WASM): ${EDGEBOX_WASM_TIME}ms avg"
fi

BUN_TIME=$(get_time "bun $SCRIPT_DIR/fib.js")
echo "  Bun: ${BUN_TIME}ms avg"

NODE_TIME=$(get_time "node $SCRIPT_DIR/fib.js")
echo "  Node.js: ${NODE_TIME}ms avg"

PORFFOR_TIME=""
if [ -n "$PORFFOR" ]; then
    PORFFOR_TIME=$(get_time "$PORFFOR $SCRIPT_DIR/fib.js")
    echo "  Porffor (porf <js>): ${PORFFOR_TIME}ms avg"
fi

# Generate markdown results
echo ""
echo "Generating results_fib.md..."
cat > "$SCRIPT_DIR/results_fib.md" << 'HEADER'
| Runtime | Computation Time | Relative |
|:---|---:|---:|
HEADER

# Calculate relative times (EdgeBox AOT as baseline)
if [ -n "$EDGEBOX_AOT_TIME" ]; then
    echo "| \`EdgeBox (AOT)\` | ${EDGEBOX_AOT_TIME}ms | **1.00** |" >> "$SCRIPT_DIR/results_fib.md"

    [ -n "$EDGEBOX_WASM_TIME" ] && echo "| \`EdgeBox (WASM)\` | ${EDGEBOX_WASM_TIME}ms | $(echo "scale=2; $EDGEBOX_WASM_TIME / $EDGEBOX_AOT_TIME" | bc)x |" >> "$SCRIPT_DIR/results_fib.md"
    [ -n "$BUN_TIME" ] && echo "| \`Bun\` | ${BUN_TIME}ms | $(echo "scale=2; $BUN_TIME / $EDGEBOX_AOT_TIME" | bc)x |" >> "$SCRIPT_DIR/results_fib.md"
    [ -n "$NODE_TIME" ] && echo "| \`Node.js\` | ${NODE_TIME}ms | $(echo "scale=2; $NODE_TIME / $EDGEBOX_AOT_TIME" | bc)x |" >> "$SCRIPT_DIR/results_fib.md"
    [ -n "$PORFFOR_TIME" ] && echo "| \`Porffor (porf <js>)\` | ${PORFFOR_TIME}ms | $(echo "scale=2; $PORFFOR_TIME / $EDGEBOX_AOT_TIME" | bc)x |" >> "$SCRIPT_DIR/results_fib.md"
fi

cat "$SCRIPT_DIR/results_fib.md"

echo ""

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 4: Iterative Sum (proves general-purpose)
# ─────────────────────────────────────────────────────────────────
echo "─────────────────────────────────────────────────────────────────"
echo "4. Iterative Sum (proves frozen interpreter is general-purpose)"
echo "─────────────────────────────────────────────────────────────────"

EXPECTED_SUM="4999950000"
echo "Validating results (expected sum(0..99999) = $EXPECTED_SUM)..."

validate_sum() {
    local name=$1
    local cmd=$2
    local output=$(eval "$cmd" 2>/dev/null | grep -E '^[0-9]+ \(' | head -1)
    local result=$(echo "$output" | grep -oE '^[0-9]+' | head -1)
    local time=$(echo "$output" | grep -oE '\([0-9.]+ms' | grep -oE '[0-9.]+' | head -1)
    if [ "$result" = "$EXPECTED_SUM" ]; then
        echo "  ✓ $name: $result (${time}ms avg)"
        return 0
    else
        echo "  ✗ $name: got '$result' (INVALID)"
        return 1
    fi
}

validate_sum "EdgeBox AOT" "$EDGEBOX $SCRIPT_DIR/tail_recursive.aot"
if [ -f "$SCRIPT_DIR/tail_recursive.wasm" ]; then
    validate_sum "EdgeBox WASM" "$WASM_RUNNER $SCRIPT_DIR/tail_recursive.wasm"
fi
validate_sum "Bun" "bun $SCRIPT_DIR/tail_recursive.js"
validate_sum "Node.js" "node $SCRIPT_DIR/tail_recursive.js"

echo ""
echo "Running benchmark..."

get_tail_time() {
    local output=$(eval "$1" 2>/dev/null | grep -E '^[0-9]+ \(' | head -1)
    echo "$output" | grep -oE '\([0-9.]+ms' | grep -oE '[0-9.]+' | head -1
}

echo ""
EDGEBOX_AOT_TAIL_TIME=$(get_tail_time "$EDGEBOX $SCRIPT_DIR/tail_recursive.aot")
echo "  EdgeBox (AOT): ${EDGEBOX_AOT_TAIL_TIME}ms avg"

EDGEBOX_WASM_TAIL_TIME=""
if [ -f "$SCRIPT_DIR/tail_recursive.wasm" ]; then
    EDGEBOX_WASM_TAIL_TIME=$(get_tail_time "$WASM_RUNNER $SCRIPT_DIR/tail_recursive.wasm")
    echo "  EdgeBox (WASM): ${EDGEBOX_WASM_TAIL_TIME}ms avg"
fi

BUN_TAIL_TIME=$(get_tail_time "bun $SCRIPT_DIR/tail_recursive.js")
echo "  Bun: ${BUN_TAIL_TIME}ms avg"

NODE_TAIL_TIME=$(get_tail_time "node $SCRIPT_DIR/tail_recursive.js")
echo "  Node.js: ${NODE_TAIL_TIME}ms avg"

# Generate markdown results
echo ""
echo "Generating results_tail_recursive.md..."
cat > "$SCRIPT_DIR/results_tail_recursive.md" << 'HEADER'
| Runtime | Computation Time | Relative |
|:---|---:|---:|
HEADER

if [ -n "$EDGEBOX_AOT_TAIL_TIME" ]; then
    echo "| \`EdgeBox (AOT)\` | ${EDGEBOX_AOT_TAIL_TIME}ms | **1.00** |" >> "$SCRIPT_DIR/results_tail_recursive.md"
    [ -n "$EDGEBOX_WASM_TAIL_TIME" ] && echo "| \`EdgeBox (WASM)\` | ${EDGEBOX_WASM_TAIL_TIME}ms | $(echo "scale=2; $EDGEBOX_WASM_TAIL_TIME / $EDGEBOX_AOT_TAIL_TIME" | bc)x |" >> "$SCRIPT_DIR/results_tail_recursive.md"
    [ -n "$BUN_TAIL_TIME" ] && echo "| \`Bun\` | ${BUN_TAIL_TIME}ms | $(echo "scale=2; $BUN_TAIL_TIME / $EDGEBOX_AOT_TAIL_TIME" | bc)x |" >> "$SCRIPT_DIR/results_tail_recursive.md"
    [ -n "$NODE_TAIL_TIME" ] && echo "| \`Node.js\` | ${NODE_TAIL_TIME}ms | $(echo "scale=2; $NODE_TAIL_TIME / $EDGEBOX_AOT_TAIL_TIME" | bc)x |" >> "$SCRIPT_DIR/results_tail_recursive.md"
fi

cat "$SCRIPT_DIR/results_tail_recursive.md"

echo ""

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 5: Daemon Warm Pod (pre-allocated instances)
# ─────────────────────────────────────────────────────────────────
if [ "$CI_MODE" = false ]; then
    echo "─────────────────────────────────────────────────────────────────"
    echo "5. Daemon Warm Pod (pre-allocated batch pool)"
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
else
    echo ""
    echo "SKIP: Daemon benchmark (CI mode)"
fi

echo ""
echo "═══════════════════════════════════════════════════════════════"
echo "                    Benchmark Complete!"
echo "═══════════════════════════════════════════════════════════════"
echo ""
echo "Results saved to:"
echo "  - $SCRIPT_DIR/results_cold_start.md"
echo "  - $SCRIPT_DIR/results_memory.md"
echo "  - $SCRIPT_DIR/results_fib.md"
echo "  - $SCRIPT_DIR/results_tail_recursive.md"
echo "  - $SCRIPT_DIR/results_daemon_warm.md"
