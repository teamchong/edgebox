#!/bin/bash
# EdgeBox Benchmark Suite
# Tests 4 runtimes: EdgeBox (AOT), EdgeBox (WASM), Bun, Node.js
# EdgeBox uses unified binary with --binary mode for accurate benchmarking
# Catches runtime failures and displays in summary, continues benchmarking
#
# Usage:
#   ./run_hyperfine.sh              # Run all benchmarks
#   ./run_hyperfine.sh --only=startup    # Run only startup benchmark
#   ./run_hyperfine.sh --only=fib        # Run only fib benchmark

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"

# Detect timeout command (GNU timeout on Linux, gtimeout on macOS with coreutils)
if command -v timeout >/dev/null 2>&1; then
    TIMEOUT_CMD="timeout"
elif command -v gtimeout >/dev/null 2>&1; then
    TIMEOUT_CMD="gtimeout"
else
    # Fallback: use Perl-based timeout (works on all Unix systems)
    TIMEOUT_CMD=""
    echo "WARNING: Neither 'timeout' nor 'gtimeout' found. Using Perl-based timeout fallback."
fi

# Parse arguments
ONLY_BENCHMARK=""
for arg in "$@"; do
    case $arg in
        --only=*)
            ONLY_BENCHMARK="${arg#*=}"
            shift
            ;;
        *)
            echo "Unknown argument: $arg"
            echo "Usage: $0 [--only=startup|memory|fib|loop|tail_recursive|typed_array]"
            exit 1
            ;;
    esac
done

# Validate --only argument
if [ -n "$ONLY_BENCHMARK" ]; then
    case "$ONLY_BENCHMARK" in
        startup|memory|fib|loop|tail_recursive|typed_array)
            ;;
        *)
            echo "ERROR: Invalid benchmark name: $ONLY_BENCHMARK"
            echo "Valid names: startup, memory, fib, loop, tail_recursive, typed_array"
            exit 1
            ;;
    esac
fi

echo "═══════════════════════════════════════════════════════════════"
if [ -n "$ONLY_BENCHMARK" ]; then
    echo "     EdgeBox Benchmark: $ONLY_BENCHMARK"
else
    echo "         EdgeBox Benchmark Suite - Fail-Fast Mode"
fi
echo "═══════════════════════════════════════════════════════════════"
echo ""

# ─────────────────────────────────────────────────────────────────
# PREREQUISITE CHECKS - Fail immediately if anything is missing
# ─────────────────────────────────────────────────────────────────
echo "Checking prerequisites..."

if [ ! -f "$ROOT_DIR/build.zig" ]; then
    echo "ERROR: Cannot find EdgeBox repo. Expected build.zig at: $ROOT_DIR/build.zig"
    exit 1
fi

# Check required commands
for cmd in bun node hyperfine bc curl nc; do
    if ! command -v $cmd &> /dev/null; then
        echo "ERROR: Required command '$cmd' not found. Install it first."
        exit 1
    fi
done

EDGEBOX="$ROOT_DIR/zig-out/bin/edgebox"
EDGEBOXC="$ROOT_DIR/zig-out/bin/edgeboxc"

# Suppress debug/info messages during benchmarks
export EDGEBOX_QUIET=1

# Set same memory limit (2 GB) for all runtimes - fair apples-to-apples comparison
# EdgeBox: configured via heap_size in .edgebox.json or source defaults (2 GB)
# Node.js: max old space size in MB
export NODE_OPTIONS="--max-old-space-size=2048"
# Bun: JSC RAM size in bytes (2 GB)
export BUN_JSC_forceRAMSize=2147483648

# Detect platform
PLATFORM=$(uname -s)
ARCH=$(uname -m)
# Platform message (WAMR with AOT+SIMD, no JIT)
if [ "$PLATFORM" = "Darwin" ] && [ "$ARCH" = "arm64" ]; then
    echo "Platform: macOS ARM64 - using native AOT+SIMD"
elif [ "$PLATFORM" = "Linux" ] && [ "$ARCH" = "x86_64" ]; then
    echo "Platform: Linux x86_64 - using native AOT+SIMD"
else
    echo "Platform: $PLATFORM $ARCH - using native AOT+SIMD"
fi

# Build CLI tools
echo "Building CLI tools..."
cd "$ROOT_DIR" && zig build cli -Doptimize=ReleaseFast

# Verify CLI tools exist
for tool in "$EDGEBOX" "$EDGEBOXC"; do
    if [ ! -x "$tool" ]; then
        echo "ERROR: Required tool not found: $tool"
        exit 1
    fi
done

echo "  All prerequisites OK"
echo ""

# ─────────────────────────────────────────────────────────────────
# BUILD BENCHMARK ARTIFACTS
# ─────────────────────────────────────────────────────────────────
echo "Building benchmark artifacts..."

# Helper function to check if benchmark should run
should_run() {
    local bench_name=$1
    if [ -z "$ONLY_BENCHMARK" ]; then
        return 0  # Run all if no filter
    fi

    # Map file names to benchmark names
    case "$bench_name" in
        hello) [ "$ONLY_BENCHMARK" = "startup" ] && return 0 ;;
        memory) [ "$ONLY_BENCHMARK" = "memory" ] && return 0 ;;
        fib) [ "$ONLY_BENCHMARK" = "fib" ] && return 0 ;;
        loop) [ "$ONLY_BENCHMARK" = "loop" ] && return 0 ;;
        tail_recursive) [ "$ONLY_BENCHMARK" = "tail_recursive" ] && return 0 ;;
        typed_array) [ "$ONLY_BENCHMARK" = "typed_array" ] && return 0 ;;
    esac
    return 1
}

build_bench() {
    local name=$1
    local js_file="bench/$name.js"
    # Build outputs go to zig-out/bin/bench/$name.js/ (edgeboxc creates subdirs)
    local wasm_file="$ROOT_DIR/zig-out/bin/bench/$name.js/$name.wasm"
    local aot_file="$ROOT_DIR/zig-out/bin/bench/$name.js/$name.aot"

    if [ ! -f "$ROOT_DIR/$js_file" ]; then
        echo "ERROR: Benchmark source not found: $ROOT_DIR/$js_file"
        exit 1
    fi

    # Skip build if outputs already exist and are newer than source
    if [ -f "$wasm_file" ] && [ -f "$aot_file" ] && \
       [ "$wasm_file" -nt "$ROOT_DIR/$js_file" ]; then
        echo "  $name: using cached build"
        return 0
    fi

    echo "  Building $name..."
    rm -f "$wasm_file" "$aot_file"
    cd "$ROOT_DIR"
    # Capture build output, show [build]/[warn]/[error] lines, fail on error
    if ! BUILD_OUTPUT=$("$EDGEBOXC" build "$js_file" 2>&1); then
        echo "ERROR: edgeboxc build failed for $js_file:"
        echo "$BUILD_OUTPUT"
        exit 1
    fi
    echo "$BUILD_OUTPUT" | grep -E '^\[build\]|\[warn\]|\[error\]' || true

    # Verify outputs were created
    if [ ! -f "$wasm_file" ]; then
        echo "ERROR: Build failed - WASM not created: $wasm_file"
        exit 1
    fi
    if [ ! -f "$aot_file" ]; then
        echo "ERROR: Build failed - AOT not created: $aot_file"
        exit 1
    fi
}

should_run hello && build_bench hello
should_run memory && build_bench memory
should_run fib && build_bench fib
should_run loop && build_bench loop
should_run tail_recursive && build_bench tail_recursive
should_run typed_array && build_bench typed_array

echo "  All artifacts built"
echo ""

# ─────────────────────────────────────────────────────────────────
# DAEMON MANAGEMENT (unified edgebox auto-starts daemon via Unix socket)
# ─────────────────────────────────────────────────────────────────
cleanup_daemons() {
    # Kill any leftover edgebox daemon processes from previous runs
    pkill -f "edgebox --serve" 2>/dev/null || true
    rm -f /tmp/edgebox-*.sock 2>/dev/null || true
}

# Cleanup on exit
trap cleanup_daemons EXIT

# ─────────────────────────────────────────────────────────────────
# HELPER FUNCTIONS
# ─────────────────────────────────────────────────────────────────

# Format time with ms suffix only for numeric values
fmt_time() {
    local t="$1"
    if [ "$t" = "TIMEOUT" ] || [ "$t" = "FAIL" ] || [ "$t" = "N/A" ]; then
        echo "$t"
    else
        echo "${t}ms"
    fi
}

# Format memory with MB suffix only for numeric values
fmt_mem() {
    local m="$1"
    if [ "$m" = "TIMEOUT" ] || [ "$m" = "FAIL" ] || [ "$m" = "N/A" ]; then
        echo "$m"
    else
        echo "${m}MB"
    fi
}

# Run command and extract timing - with timeout (same for ALL runtimes)
BENCH_TIMEOUT=120  # 2 minutes per benchmark run

get_time() {
    local output
    local exit_code

    # Execute command with timeout (GNU timeout, gtimeout, or Perl fallback)
    if [ -n "$TIMEOUT_CMD" ]; then
        # Use GNU timeout or gtimeout
        output=$($TIMEOUT_CMD $BENCH_TIMEOUT "$@" 2>&1)
        exit_code=$?
    else
        # Perl-based timeout fallback (works on all Unix systems including macOS)
        output=$(perl -e 'alarm shift @ARGV; exec @ARGV' "$BENCH_TIMEOUT" "$@" 2>&1)
        exit_code=$?
    fi

    if [ $exit_code -eq 124 ] || [ $exit_code -eq 142 ]; then
        # 124 = GNU timeout exit code, 142 = SIGALRM (Perl alarm)
        echo "[BENCHMARK ERROR] Command timed out after ${BENCH_TIMEOUT}s: $*" >&2
        echo "TIMEOUT"
        return 0
    elif [ $exit_code -ne 0 ]; then
        echo "[BENCHMARK ERROR] Command failed with exit code $exit_code: $*" >&2
        echo "[BENCHMARK ERROR] Output: $output" >&2
        echo "FAIL"
        return 0
    fi
    local time=$(echo "$output" | grep -oE '\([0-9.]+ms' | grep -oE '[0-9.]+' | head -1)
    if [ -z "$time" ]; then
        echo "[BENCHMARK ERROR] Could not parse timing from output" >&2
        echo "[BENCHMARK ERROR] Command was: $*" >&2
        echo "[BENCHMARK ERROR] Output was:" >&2
        echo "$output" >&2
        echo "[BENCHMARK ERROR] Trying grep:" >&2
        echo "$output" | grep -E '\(' >&2 || echo "  No parentheses found" >&2
        echo "FAIL"
        return 0
    fi
    echo "$time"
}

# Get file size - FAIL if file missing
get_size() {
    local file=$1
    if [ ! -f "$file" ]; then
        echo "ERROR: File not found: $file"
        exit 1
    fi
    ls -lh "$file" | awk '{print $5}'
}

# Get memory usage - with timeout (same for ALL runtimes)
get_mem() {
    local output
    local bytes
    if [ "$PLATFORM" = "Darwin" ]; then
        output=$(timeout $BENCH_TIMEOUT /usr/bin/time -l "$@" 2>&1)
        local exit_code=$?
        if [ $exit_code -eq 124 ]; then
            echo "[BENCHMARK ERROR] Command timed out after ${BENCH_TIMEOUT}s: $*" >&2
            echo "TIMEOUT"
            return 0
        elif [ $exit_code -ne 0 ]; then
            echo "[BENCHMARK ERROR] Command failed with exit code $exit_code: $*" >&2
            echo "[BENCHMARK ERROR] Output: $output" >&2
            echo "FAIL"
            return 0
        fi
        bytes=$(echo "$output" | grep "maximum resident set size" | awk 'NF{print $1}')
    else
        output=$(timeout $BENCH_TIMEOUT /usr/bin/time -v "$@" 2>&1)
        local exit_code=$?
        if [ $exit_code -eq 124 ]; then
            echo "[BENCHMARK ERROR] Command timed out after ${BENCH_TIMEOUT}s: $*" >&2
            echo "TIMEOUT"
            return 0
        elif [ $exit_code -ne 0 ]; then
            echo "[BENCHMARK ERROR] Command failed with exit code $exit_code: $*" >&2
            echo "[BENCHMARK ERROR] Output: $output" >&2
            echo "FAIL"
            return 0
        fi
        bytes=$(echo "$output" | grep "Maximum resident set size" | awk '{print $NF}')
        bytes=$((bytes * 1024))
    fi
    if [ -z "$bytes" ] || [ "$bytes" = "0" ]; then
        echo "[BENCHMARK ERROR] Could not parse memory from output" >&2
        echo "[BENCHMARK ERROR] Command was: $*" >&2
        echo "[BENCHMARK ERROR] Output was:" >&2
        echo "$output" >&2
        echo "[BENCHMARK ERROR] Searching for 'maximum resident set size':" >&2
        echo "$output" | grep -i "resident" >&2 || echo "  Not found" >&2
        echo "FAIL"
        return 0
    fi
    echo "scale=1; $bytes / 1024 / 1024" | bc
}

# ─────────────────────────────────────────────────────────────────
# BENCHMARK PARAMETERS
# ─────────────────────────────────────────────────────────────────
BENCH_RUNS=5
BENCH_WARMUP=1

echo "═══════════════════════════════════════════════════════════════"
echo "                    Running Benchmarks"
echo "═══════════════════════════════════════════════════════════════"
echo ""

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 1: Startup Time (hello.js)
# Tests: AOT (daemon mode), WASM (daemon mode), Bun, Node.js
# Note: edgebox auto-starts daemon via Unix socket for fast warm starts
# ─────────────────────────────────────────────────────────────────
if should_run hello; then
echo "─────────────────────────────────────────────────────────────────"
echo "1. Startup Time (hello.js) - ALL 4 RUNTIMES"
echo "─────────────────────────────────────────────────────────────────"

AOT_FILE="$ROOT_DIR/zig-out/bin/bench/hello.js/hello.aot"
WASM_FILE="$ROOT_DIR/zig-out/bin/bench/hello.js/hello.wasm"
JS_FILE="$SCRIPT_DIR/hello.js"

echo "  File sizes:"
echo "    AOT:  $(get_size $AOT_FILE)"
echo "    WASM: $(get_size $WASM_FILE)"
echo "    JS:   $(get_size $JS_FILE)"
echo ""

# Clean up any existing daemons before starting
cleanup_daemons

# Build hyperfine command with ALL 4 runtimes
# EdgeBox uses daemon mode (auto-starts daemon via Unix socket)
HYPERFINE_CMD="hyperfine --warmup $BENCH_WARMUP --runs $BENCH_RUNS"
HYPERFINE_CMD+=" -n 'EdgeBox (AOT)' '$EDGEBOX $AOT_FILE'"
HYPERFINE_CMD+=" -n 'EdgeBox (WASM)' '$EDGEBOX $WASM_FILE'"
HYPERFINE_CMD+=" -n 'Bun' 'bun $JS_FILE'"
HYPERFINE_CMD+=" -n 'Node.js' 'node $JS_FILE'"
HYPERFINE_CMD+=" --export-markdown '$SCRIPT_DIR/results_startup.md'"

eval $HYPERFINE_CMD
cleanup_daemons
echo ""
fi

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 2: Memory Usage (600k objects)
# Tests: AOT, WASM, Bun, Node.js
# Uses --binary mode for accurate memory measurement
# ─────────────────────────────────────────────────────────────────
if should_run memory; then
echo "─────────────────────────────────────────────────────────────────"
echo "2. Memory Usage (600k objects) - ALL 4 RUNTIMES"
echo "─────────────────────────────────────────────────────────────────"

AOT_FILE="$ROOT_DIR/zig-out/bin/bench/memory.js/memory.aot"
WASM_FILE="$ROOT_DIR/zig-out/bin/bench/memory.js/memory.wasm"
JS_FILE="$SCRIPT_DIR/memory.js"

# Use --binary mode for memory measurement (avoids daemon overhead)
MEM_AOT=$(get_mem $EDGEBOX --binary $AOT_FILE)
MEM_WASM=$(get_mem $EDGEBOX --binary $WASM_FILE)
MEM_BUN=$(get_mem bun $JS_FILE)
MEM_NODE=$(get_mem node $JS_FILE)

echo "  EdgeBox (AOT):    $(fmt_mem "$MEM_AOT")"
echo "  EdgeBox (WASM):   $(fmt_mem "$MEM_WASM")"
echo "  Bun:              $(fmt_mem "$MEM_BUN")"
echo "  Node.js:          $(fmt_mem "$MEM_NODE")"

cat > "$SCRIPT_DIR/results_memory.md" << EOF
| Runtime | Memory |
|:---|---:|
| EdgeBox (AOT) | $(fmt_mem "$MEM_AOT") |
| EdgeBox (WASM) | $(fmt_mem "$MEM_WASM") |
| Bun | $(fmt_mem "$MEM_BUN") |
| Node.js | $(fmt_mem "$MEM_NODE") |
EOF

echo ""
fi

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 3: Fibonacci fib(45) - frozen recursive
# Tests: AOT, WASM, Bun, Node.js
# Uses --binary mode to measure pure execution time
# ─────────────────────────────────────────────────────────────────
if should_run fib; then
echo "─────────────────────────────────────────────────────────────────"
echo "3. Fibonacci fib(45) - frozen recursive - ALL 4 RUNTIMES"
echo "─────────────────────────────────────────────────────────────────"

AOT_FILE="$ROOT_DIR/zig-out/bin/bench/fib.js/fib.aot"
WASM_FILE="$ROOT_DIR/zig-out/bin/bench/fib.js/fib.wasm"
JS_FILE="$SCRIPT_DIR/fib.js"

# Use --binary mode to measure pure execution time (no daemon startup overhead)
EDGEBOX_AOT_TIME=$(get_time $EDGEBOX --binary $AOT_FILE)
EDGEBOX_WASM_TIME=$(get_time $EDGEBOX --binary $WASM_FILE)
BUN_TIME=$(get_time bun $JS_FILE)
NODE_TIME=$(get_time node $JS_FILE)

echo "  EdgeBox (AOT):    $(fmt_time "$EDGEBOX_AOT_TIME")"
echo "  EdgeBox (WASM):   $(fmt_time "$EDGEBOX_WASM_TIME")"
echo "  Bun:              $(fmt_time "$BUN_TIME")"
echo "  Node.js:          $(fmt_time "$NODE_TIME")"

cat > "$SCRIPT_DIR/results_fib.md" << EOF
| Runtime | Time |
|:---|---:|
| EdgeBox (AOT) | $(fmt_time "$EDGEBOX_AOT_TIME") |
| EdgeBox (WASM) | $(fmt_time "$EDGEBOX_WASM_TIME") |
| Bun | $(fmt_time "$BUN_TIME") |
| Node.js | $(fmt_time "$NODE_TIME") |
EOF

echo ""
fi

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 4: Loop (array sum) - frozen array iteration
# Tests: AOT, WASM, Bun, Node.js
# ─────────────────────────────────────────────────────────────────
if should_run loop; then
echo "─────────────────────────────────────────────────────────────────"
echo "4. Loop (array sum) - frozen array iteration - ALL 4 RUNTIMES"
echo "─────────────────────────────────────────────────────────────────"

AOT_FILE="$ROOT_DIR/zig-out/bin/bench/loop.js/loop.aot"
WASM_FILE="$ROOT_DIR/zig-out/bin/bench/loop.js/loop.wasm"
JS_FILE="$SCRIPT_DIR/loop.js"

EDGEBOX_AOT_TIME=$(get_time $EDGEBOX --binary $AOT_FILE)
EDGEBOX_WASM_TIME=$(get_time $EDGEBOX --binary $WASM_FILE)
BUN_TIME=$(get_time bun $JS_FILE)
NODE_TIME=$(get_time node $JS_FILE)

echo "  EdgeBox (AOT):    $(fmt_time "$EDGEBOX_AOT_TIME")"
echo "  EdgeBox (WASM):   $(fmt_time "$EDGEBOX_WASM_TIME")"
echo "  Bun:              $(fmt_time "$BUN_TIME")"
echo "  Node.js:          $(fmt_time "$NODE_TIME")"

cat > "$SCRIPT_DIR/results_loop.md" << EOF
| Runtime | Time |
|:---|---:|
| EdgeBox (AOT) | $(fmt_time "$EDGEBOX_AOT_TIME") |
| EdgeBox (WASM) | $(fmt_time "$EDGEBOX_WASM_TIME") |
| Bun | $(fmt_time "$BUN_TIME") |
| Node.js | $(fmt_time "$NODE_TIME") |
EOF

echo ""
fi

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 5: Tail Recursive - function call overhead
# Tests: AOT, WASM, Bun, Node.js
# ─────────────────────────────────────────────────────────────────
if should_run tail_recursive; then
echo "─────────────────────────────────────────────────────────────────"
echo "5. Tail Recursive - function call overhead - ALL 4 RUNTIMES"
echo "─────────────────────────────────────────────────────────────────"

AOT_FILE="$ROOT_DIR/zig-out/bin/bench/tail_recursive.js/tail_recursive.aot"
WASM_FILE="$ROOT_DIR/zig-out/bin/bench/tail_recursive.js/tail_recursive.wasm"
JS_FILE="$SCRIPT_DIR/tail_recursive.js"

EDGEBOX_AOT_TIME=$(get_time $EDGEBOX --binary $AOT_FILE)
EDGEBOX_WASM_TIME=$(get_time $EDGEBOX --binary $WASM_FILE)
BUN_TIME=$(get_time bun $JS_FILE)
NODE_TIME=$(get_time node $JS_FILE)

echo "  EdgeBox (AOT):    $(fmt_time "$EDGEBOX_AOT_TIME")"
echo "  EdgeBox (WASM):   $(fmt_time "$EDGEBOX_WASM_TIME")"
echo "  Bun:              $(fmt_time "$BUN_TIME")"
echo "  Node.js:          $(fmt_time "$NODE_TIME")"

cat > "$SCRIPT_DIR/results_tail_recursive.md" << EOF
| Runtime | Time |
|:---|---:|
| EdgeBox (AOT) | $(fmt_time "$EDGEBOX_AOT_TIME") |
| EdgeBox (WASM) | $(fmt_time "$EDGEBOX_WASM_TIME") |
| Bun | $(fmt_time "$BUN_TIME") |
| Node.js | $(fmt_time "$NODE_TIME") |
EOF

echo ""
fi

# ─────────────────────────────────────────────────────────────────
# 6. TYPED ARRAY - Int32Array with direct buffer access
# Tests: AOT, WASM, Bun, Node.js
# ─────────────────────────────────────────────────────────────────
if should_run typed_array; then
echo "─────────────────────────────────────────────────────────────────"
echo "6. TypedArray (Int32Array sum) - direct buffer access - ALL 4 RUNTIMES"
echo "─────────────────────────────────────────────────────────────────"

AOT_FILE="$ROOT_DIR/zig-out/bin/bench/typed_array.js/typed_array.aot"
WASM_FILE="$ROOT_DIR/zig-out/bin/bench/typed_array.js/typed_array.wasm"
JS_FILE="$SCRIPT_DIR/typed_array.js"

EDGEBOX_AOT_TIME=$(get_time $EDGEBOX --binary $AOT_FILE)
EDGEBOX_WASM_TIME=$(get_time $EDGEBOX --binary $WASM_FILE)
BUN_TIME=$(get_time bun $JS_FILE)
NODE_TIME=$(get_time node $JS_FILE)

echo "  EdgeBox (AOT):    $(fmt_time "$EDGEBOX_AOT_TIME")"
echo "  EdgeBox (WASM):   $(fmt_time "$EDGEBOX_WASM_TIME")"
echo "  Bun:              $(fmt_time "$BUN_TIME")"
echo "  Node.js:          $(fmt_time "$NODE_TIME")"

cat > "$SCRIPT_DIR/results_typed_array.md" << EOF
| Runtime | Time |
|:---|---:|
| EdgeBox (AOT) | $(fmt_time "$EDGEBOX_AOT_TIME") |
| EdgeBox (WASM) | $(fmt_time "$EDGEBOX_WASM_TIME") |
| Bun | $(fmt_time "$BUN_TIME") |
| Node.js | $(fmt_time "$NODE_TIME") |
EOF

echo ""
fi

# ─────────────────────────────────────────────────────────────────
# SUMMARY
# ─────────────────────────────────────────────────────────────────
echo "═══════════════════════════════════════════════════════════════"
echo "                    Benchmark Complete!"
echo "═══════════════════════════════════════════════════════════════"
echo ""
echo "Results saved to:"
echo "  - $SCRIPT_DIR/results_startup.md"
echo "  - $SCRIPT_DIR/results_fib.md"
echo "  - $SCRIPT_DIR/results_loop.md"
echo "  - $SCRIPT_DIR/results_tail_recursive.md"
echo "  - $SCRIPT_DIR/results_typed_array.md"
echo ""
echo "Runtimes tested: EdgeBox (AOT, WASM), Bun, Node.js"

# ─────────────────────────────────────────────────────────────────
# NOTE: GitHub Actions summaries are handled by the workflow's summary job
# Each benchmark job uploads its results as artifacts, and the summary job
# combines them into one unified summary. Individual jobs should NOT write
# to GITHUB_STEP_SUMMARY to avoid duplicate summaries.
# ─────────────────────────────────────────────────────────────────
