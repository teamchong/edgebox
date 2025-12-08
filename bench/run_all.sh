#!/bin/bash
# EdgeBox Benchmark Suite
# Measures cold start, execution speed, and memory usage
# Compares against wasmedge-quickjs, Porffor, Bun, and Node.js

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
ITERATIONS=${1:-100}

# Check for gdate (GNU date) on macOS for nanosecond precision
if command -v gdate &> /dev/null; then
    DATE_CMD="gdate"
elif date +%N | grep -q N; then
    echo "Warning: date doesn't support nanoseconds, using millisecond precision"
    DATE_CMD="date"
else
    DATE_CMD="date"
fi

# Get time in nanoseconds (or milliseconds as fallback)
get_time_ns() {
    if [[ "$DATE_CMD" == "gdate" ]] || [[ "$(date +%N)" != "N" ]]; then
        $DATE_CMD +%s%N
    else
        echo $(($(date +%s) * 1000000000))
    fi
}

# Calculate percentiles from sorted array
calc_percentile() {
    local -n arr=$1
    local pct=$2
    local idx=$((${#arr[@]} * pct / 100))
    echo "${arr[$idx]}"
}

echo "═══════════════════════════════════════════════════════════════"
echo "                    EdgeBox Benchmark Suite"
echo "═══════════════════════════════════════════════════════════════"
echo "Iterations: $ITERATIONS"
echo ""

# Build EdgeBox first
echo "Building EdgeBox..."
cd "$ROOT_DIR"
./build.sh > /dev/null 2>&1 || { echo "Build failed!"; exit 1; }
echo "Build complete."
echo ""

# Porffor path (npm global install location)
PORFFOR="$HOME/.local/share/mise/installs/node/20.18.0/lib/node_modules/porffor/porf"
if [ ! -x "$PORFFOR" ]; then
    # Try standard npm global location
    PORFFOR="$(npm root -g 2>/dev/null)/porffor/porf"
fi

# Define runtimes to test
declare -A RUNTIMES
RUNTIMES["EdgeBox"]="$ROOT_DIR/run.sh"  # Pool allocator + AOT by default

# Check for other runtimes
# wasmedge-quickjs (with AOT-compiled version)
if command -v wasmedge &> /dev/null; then
    if [[ -f "$HOME/.wasmedge/lib/wasmedge_quickjs_aot.wasm" ]]; then
        RUNTIMES["wasmedge-quickjs"]="WASMEDGE_QJS_AOT"  # Special marker, handled in run_runtime
    elif [[ -f "$HOME/.wasmedge/lib/wasmedge_quickjs.wasm" ]]; then
        RUNTIMES["wasmedge-quickjs"]="WASMEDGE_QJS"  # Non-AOT fallback
    fi
fi
# Porffor AOT compiler
if [ -x "$PORFFOR" ]; then
    RUNTIMES["Porffor"]="PORFFOR"  # Special marker, handled in run_runtime
fi
if command -v bun &> /dev/null; then
    RUNTIMES["Bun"]="bun"
fi
if command -v node &> /dev/null; then
    RUNTIMES["Node"]="node"
fi

# Helper function to run a script with a runtime
run_runtime() {
    local runtime_key="$1"
    local script="$2"
    local cmd="${RUNTIMES[$runtime_key]}"
    local script_dir
    script_dir=$(dirname "$script")

    case "$cmd" in
        "WASMEDGE_QJS_AOT")
            wasmedge --dir "$script_dir" "$HOME/.wasmedge/lib/wasmedge_quickjs_aot.wasm" "$script"
            ;;
        "WASMEDGE_QJS")
            wasmedge --dir "$script_dir" "$HOME/.wasmedge/lib/wasmedge_quickjs.wasm" "$script"
            ;;
        "PORFFOR")
            "$PORFFOR" "$script"
            ;;
        *)
            $cmd "$script"
            ;;
    esac
}

echo "Runtimes available: ${!RUNTIMES[@]}"
echo ""

# Cold Start Benchmark
echo "─────────────────────────────────────────────────────────────────"
echo "Cold Start (P50/P90/P99 in ms)"
echo "─────────────────────────────────────────────────────────────────"

for runtime in "${!RUNTIMES[@]}"; do
    results=()

    for ((i=0; i<ITERATIONS; i++)); do
        start=$(get_time_ns)
        run_runtime "$runtime" "$SCRIPT_DIR/hello.js" > /dev/null 2>&1 || true
        end=$(get_time_ns)
        ms=$(( (end - start) / 1000000 ))
        results+=($ms)
    done

    # Sort results
    IFS=$'\n' sorted=($(sort -n <<<"${results[*]}")); unset IFS

    p50=$(calc_percentile sorted 50)
    p90=$(calc_percentile sorted 90)
    p99=$(calc_percentile sorted 99)

    printf "%-20s P50=%4dms  P90=%4dms  P99=%4dms\n" "$runtime" "$p50" "$p90" "$p99"
done

echo ""

# Fibonacci Benchmark (execution speed)
echo "─────────────────────────────────────────────────────────────────"
echo "Fibonacci(35) Execution Speed"
echo "─────────────────────────────────────────────────────────────────"

for runtime in "${!RUNTIMES[@]}"; do
    echo -n "$runtime: "
    run_runtime "$runtime" "$SCRIPT_DIR/fib.js" 2>/dev/null || echo "FAILED"
done

echo ""

# Allocator Stress Test
echo "─────────────────────────────────────────────────────────────────"
echo "Allocator Stress Test (30k allocations)"
echo "─────────────────────────────────────────────────────────────────"

for runtime in "${!RUNTIMES[@]}"; do
    echo -n "$runtime: "
    run_runtime "$runtime" "$SCRIPT_DIR/alloc_stress.js" 2>/dev/null || echo "FAILED"
done

echo ""

# Memory Usage (EdgeBox only)
echo "─────────────────────────────────────────────────────────────────"
echo "Peak Memory Usage (hello.js)"
echo "─────────────────────────────────────────────────────────────────"

if [[ "$(uname)" == "Darwin" ]]; then
    for runtime in "EdgeBox" "EdgeBox+Pool"; do
        if [[ -v "RUNTIMES[$runtime]" ]]; then
            cmd="${RUNTIMES[$runtime]}"
            mem=$(/usr/bin/time -l $cmd "$SCRIPT_DIR/hello.js" 2>&1 | grep "maximum resident" | awk '{print int($1/1024)}')
            printf "%-20s %d KB\n" "$runtime" "$mem"
        fi
    done
else
    echo "Memory measurement only available on macOS"
fi

echo ""
echo "═══════════════════════════════════════════════════════════════"
echo "Benchmark complete!"
