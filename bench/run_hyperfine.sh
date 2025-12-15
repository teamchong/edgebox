#!/bin/bash
# EdgeBox Benchmark Suite
# Tests ALL 6 runtimes: EdgeBox (AOT), EdgeBox (WASM), EdgeBox (daemon), Bun, Node.js, Porffor
# Catches runtime failures and displays in summary, continues benchmarking
#
# Usage:
#   ./run_hyperfine.sh              # Run all benchmarks
#   ./run_hyperfine.sh --only=startup    # Run only startup benchmark
#   ./run_hyperfine.sh --only=fib        # Run only fib benchmark

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"

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
            echo "Usage: $0 [--only=startup|memory|fib|loop|tail_recursive]"
            exit 1
            ;;
    esac
done

# Validate --only argument
if [ -n "$ONLY_BENCHMARK" ]; then
    case "$ONLY_BENCHMARK" in
        startup|memory|fib|loop|tail_recursive)
            ;;
        *)
            echo "ERROR: Invalid benchmark name: $ONLY_BENCHMARK"
            echo "Valid names: startup, memory, fib, loop, tail_recursive"
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

# Porffor - required, same timeout rules as all other runtimes
PORFFOR=""
if [ -x "$HOME/.local/share/mise/installs/node/20.18.0/lib/node_modules/porffor/porf" ]; then
    PORFFOR="$HOME/.local/share/mise/installs/node/20.18.0/lib/node_modules/porffor/porf"
elif command -v porf &> /dev/null; then
    PORFFOR="porf"
else
    echo "ERROR: Porffor not found. Install with: npm install -g porffor"
    exit 1
fi

EDGEBOX="$ROOT_DIR/zig-out/bin/edgebox"
EDGEBOXC="$ROOT_DIR/zig-out/bin/edgeboxc"
EDGEBOXD="$ROOT_DIR/zig-out/bin/edgeboxd"

# Suppress debug/info messages during benchmarks
export EDGEBOX_QUIET=1

# Detect platform
PLATFORM=$(uname -s)
ARCH=$(uname -m)
WASM_RUNNER="$EDGEBOX"

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
for tool in "$EDGEBOX" "$EDGEBOXC" "$EDGEBOXD"; do
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
    esac
    return 1
}

build_bench() {
    local name=$1
    local js_file="bench/$name.js"
    # Build outputs go to zig-out/bin/bench/ (matching source path structure)
    local wasm_file="$ROOT_DIR/zig-out/bin/bench/$name.wasm"
    local aot_file="$ROOT_DIR/zig-out/bin/bench/$name.aot"

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

echo "  All artifacts built"
echo ""

# ─────────────────────────────────────────────────────────────────
# DAEMON MANAGEMENT
# ─────────────────────────────────────────────────────────────────
DAEMON_PORT=18080
DAEMON_PID=""

start_daemon() {
    local dylib_file=$1
    echo "  Starting daemon..."

    if [ ! -f "$dylib_file" ]; then
        echo "ERROR: Daemon dylib not found: $dylib_file"
        exit 1
    fi

    # Suppress daemon debug output
    "$EDGEBOXD" "$dylib_file" --port=$DAEMON_PORT >/dev/null 2>&1 &
    DAEMON_PID=$!
    sleep 0.5

    # Verify daemon started
    if ! printf "GET / HTTP/1.0\r\n\r\n" | nc -w1 localhost $DAEMON_PORT > /dev/null; then
        echo "ERROR: Daemon failed to start on port $DAEMON_PORT"
        kill $DAEMON_PID 2>/dev/null || true
        exit 1
    fi
    echo "  Daemon started (PID: $DAEMON_PID)"
}

stop_daemon() {
    if [ -n "$DAEMON_PID" ]; then
        echo "  Stopping daemon (PID: $DAEMON_PID)..."
        kill $DAEMON_PID 2>/dev/null || true
        wait $DAEMON_PID 2>/dev/null || true
        DAEMON_PID=""
        sleep 0.2
    fi
}

# Cleanup on exit
trap stop_daemon EXIT

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
    local cmd="$1"
    local output
    output=$(timeout $BENCH_TIMEOUT bash -c "$cmd" 2>&1)
    local exit_code=$?
    if [ $exit_code -eq 124 ]; then
        echo "TIMEOUT"
        return 0
    elif [ $exit_code -ne 0 ]; then
        echo "FAIL"
        return 0
    fi
    local time=$(echo "$output" | grep -oE '\([0-9.]+ms' | grep -oE '[0-9.]+' | head -1)
    if [ -z "$time" ]; then
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
            echo "TIMEOUT"
            return 0
        elif [ $exit_code -ne 0 ]; then
            echo "FAIL"
            return 0
        fi
        bytes=$(echo "$output" | grep "maximum resident set size" | awk 'NF{print $1}')
    else
        output=$(timeout $BENCH_TIMEOUT /usr/bin/time -v "$@" 2>&1)
        local exit_code=$?
        if [ $exit_code -eq 124 ]; then
            echo "TIMEOUT"
            return 0
        elif [ $exit_code -ne 0 ]; then
            echo "FAIL"
            return 0
        fi
        bytes=$(echo "$output" | grep "Maximum resident set size" | awk '{print $NF}')
        bytes=$((bytes * 1024))
    fi
    if [ -z "$bytes" ] || [ "$bytes" = "0" ]; then
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
# Tests: AOT, WASM, daemon (warm), Bun, Node.js, Porffor
# ─────────────────────────────────────────────────────────────────
if should_run hello; then
echo "─────────────────────────────────────────────────────────────────"
echo "1. Startup Time (hello.js) - ALL 6 RUNTIMES"
echo "─────────────────────────────────────────────────────────────────"

AOT_FILE="$ROOT_DIR/zig-out/bin/bench/hello.aot"
WASM_FILE="$ROOT_DIR/zig-out/bin/bench/hello.wasm"
JS_FILE="$SCRIPT_DIR/hello.js"

echo "  File sizes:"
echo "    AOT:  $(get_size $AOT_FILE)"
echo "    WASM: $(get_size $WASM_FILE)"
echo "    JS:   $(get_size $JS_FILE)"
echo ""

# Start daemon for this benchmark
start_daemon "$AOT_FILE"

# Build hyperfine command with ALL 6 runtimes
HYPERFINE_CMD="hyperfine --warmup $BENCH_WARMUP --runs $BENCH_RUNS"
HYPERFINE_CMD+=" -n 'EdgeBox (AOT)' '$EDGEBOX $AOT_FILE'"
HYPERFINE_CMD+=" -n 'EdgeBox (WASM)' '$WASM_RUNNER $WASM_FILE'"
HYPERFINE_CMD+=" -n 'EdgeBox (daemon)' 'curl -s http://localhost:$DAEMON_PORT/'"
HYPERFINE_CMD+=" -n 'Bun' 'bun $JS_FILE'"
HYPERFINE_CMD+=" -n 'Node.js' 'node $JS_FILE'"
HYPERFINE_CMD+=" -n 'Porffor' '$PORFFOR $JS_FILE'"
HYPERFINE_CMD+=" --export-markdown '$SCRIPT_DIR/results_startup.md'"

eval $HYPERFINE_CMD
stop_daemon
echo ""
fi

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 2: Memory Usage (600k objects)
# Tests: AOT, WASM, daemon, Bun, Node.js, Porffor
# ─────────────────────────────────────────────────────────────────
if should_run memory; then
echo "─────────────────────────────────────────────────────────────────"
echo "2. Memory Usage (600k objects) - ALL 6 RUNTIMES"
echo "─────────────────────────────────────────────────────────────────"

AOT_FILE="$ROOT_DIR/zig-out/bin/bench/memory.aot"
WASM_FILE="$ROOT_DIR/zig-out/bin/bench/memory.wasm"
JS_FILE="$SCRIPT_DIR/memory.js"

start_daemon "$AOT_FILE"

MEM_AOT=$(get_mem $EDGEBOX $AOT_FILE)
MEM_WASM=$(get_mem $WASM_RUNNER $WASM_FILE)
MEM_BUN=$(get_mem bun $JS_FILE)
MEM_NODE=$(get_mem node $JS_FILE)
MEM_PORFFOR=$(get_mem $PORFFOR $JS_FILE)

echo "  EdgeBox (AOT):    $(fmt_mem "$MEM_AOT")"
echo "  EdgeBox (WASM):   $(fmt_mem "$MEM_WASM")"
echo "  EdgeBox (daemon): (shared memory with daemon process)"
echo "  Bun:              $(fmt_mem "$MEM_BUN")"
echo "  Node.js:          $(fmt_mem "$MEM_NODE")"
echo "  Porffor:          $(fmt_mem "$MEM_PORFFOR")"

cat > "$SCRIPT_DIR/results_memory.md" << EOF
| Runtime | Memory |
|:---|---:|
| EdgeBox (AOT) | $(fmt_mem "$MEM_AOT") |
| EdgeBox (WASM) | $(fmt_mem "$MEM_WASM") |
| Bun | $(fmt_mem "$MEM_BUN") |
| Node.js | $(fmt_mem "$MEM_NODE") |
| Porffor | $(fmt_mem "$MEM_PORFFOR") |
EOF

stop_daemon
echo ""
fi

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 3: Fibonacci fib(45) - frozen recursive
# Tests: AOT, WASM, daemon, Bun, Node.js, Porffor
# ─────────────────────────────────────────────────────────────────
if should_run fib; then
echo "─────────────────────────────────────────────────────────────────"
echo "3. Fibonacci fib(45) - frozen recursive - ALL 6 RUNTIMES"
echo "─────────────────────────────────────────────────────────────────"

AOT_FILE="$ROOT_DIR/zig-out/bin/bench/fib.aot"
WASM_FILE="$ROOT_DIR/zig-out/bin/bench/fib.wasm"
JS_FILE="$SCRIPT_DIR/fib.js"

start_daemon "$AOT_FILE"

EDGEBOX_AOT_TIME=$(get_time "$EDGEBOX $AOT_FILE")
EDGEBOX_WASM_TIME=$(get_time "$WASM_RUNNER $WASM_FILE")
# Daemon runs fib on request - parse timing from output (formats: "XXXms" or "(XXXms)")
DAEMON_OUTPUT=$(curl -s http://localhost:$DAEMON_PORT/)
EDGEBOX_DAEMON_TIME=$(echo "$DAEMON_OUTPUT" | grep -oE '[0-9.]+ms' | grep -oE '[0-9.]+' | head -1)
if [ -z "$EDGEBOX_DAEMON_TIME" ]; then
    echo "WARNING: Could not parse daemon timing from: $DAEMON_OUTPUT"
    EDGEBOX_DAEMON_TIME="N/A"
fi
BUN_TIME=$(get_time "bun $JS_FILE")
NODE_TIME=$(get_time "node $JS_FILE")
PORFFOR_TIME=$(get_time "$PORFFOR $JS_FILE")

echo "  EdgeBox (AOT):    $(fmt_time "$EDGEBOX_AOT_TIME")"
echo "  EdgeBox (WASM):   $(fmt_time "$EDGEBOX_WASM_TIME")"
echo "  EdgeBox (daemon): $(fmt_time "$EDGEBOX_DAEMON_TIME")"
echo "  Bun:              $(fmt_time "$BUN_TIME")"
echo "  Node.js:          $(fmt_time "$NODE_TIME")"
echo "  Porffor:          $(fmt_time "$PORFFOR_TIME")"

cat > "$SCRIPT_DIR/results_fib.md" << EOF
| Runtime | Time |
|:---|---:|
| EdgeBox (AOT) | $(fmt_time "$EDGEBOX_AOT_TIME") |
| EdgeBox (WASM) | $(fmt_time "$EDGEBOX_WASM_TIME") |
| EdgeBox (daemon) | $(fmt_time "$EDGEBOX_DAEMON_TIME") |
| Bun | $(fmt_time "$BUN_TIME") |
| Node.js | $(fmt_time "$NODE_TIME") |
| Porffor | $(fmt_time "$PORFFOR_TIME") |
EOF

stop_daemon
echo ""
fi

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 4: Loop (array sum) - frozen array iteration
# Tests: AOT, WASM, daemon, Bun, Node.js, Porffor
# ─────────────────────────────────────────────────────────────────
if should_run loop; then
echo "─────────────────────────────────────────────────────────────────"
echo "4. Loop (array sum) - frozen array iteration - ALL 6 RUNTIMES"
echo "─────────────────────────────────────────────────────────────────"

AOT_FILE="$ROOT_DIR/zig-out/bin/bench/loop.aot"
WASM_FILE="$ROOT_DIR/zig-out/bin/bench/loop.wasm"
JS_FILE="$SCRIPT_DIR/loop.js"

start_daemon "$AOT_FILE"

EDGEBOX_AOT_TIME=$(get_time "$EDGEBOX $AOT_FILE")
EDGEBOX_WASM_TIME=$(get_time "$WASM_RUNNER $WASM_FILE")
DAEMON_OUTPUT=$(curl -s http://localhost:$DAEMON_PORT/)
EDGEBOX_DAEMON_TIME=$(echo "$DAEMON_OUTPUT" | grep -oE '[0-9.]+ms' | grep -oE '[0-9.]+' | head -1)
if [ -z "$EDGEBOX_DAEMON_TIME" ]; then
    echo "WARNING: Could not parse daemon timing from: $DAEMON_OUTPUT"
    EDGEBOX_DAEMON_TIME="N/A"
fi
BUN_TIME=$(get_time "bun $JS_FILE")
NODE_TIME=$(get_time "node $JS_FILE")
PORFFOR_TIME=$(get_time "$PORFFOR $JS_FILE")

echo "  EdgeBox (AOT):    $(fmt_time "$EDGEBOX_AOT_TIME")"
echo "  EdgeBox (WASM):   $(fmt_time "$EDGEBOX_WASM_TIME")"
echo "  EdgeBox (daemon): $(fmt_time "$EDGEBOX_DAEMON_TIME")"
echo "  Bun:              $(fmt_time "$BUN_TIME")"
echo "  Node.js:          $(fmt_time "$NODE_TIME")"
echo "  Porffor:          $(fmt_time "$PORFFOR_TIME")"

cat > "$SCRIPT_DIR/results_loop.md" << EOF
| Runtime | Time |
|:---|---:|
| EdgeBox (AOT) | $(fmt_time "$EDGEBOX_AOT_TIME") |
| EdgeBox (WASM) | $(fmt_time "$EDGEBOX_WASM_TIME") |
| EdgeBox (daemon) | $(fmt_time "$EDGEBOX_DAEMON_TIME") |
| Bun | $(fmt_time "$BUN_TIME") |
| Node.js | $(fmt_time "$NODE_TIME") |
| Porffor | $(fmt_time "$PORFFOR_TIME") |
EOF

stop_daemon
echo ""
fi

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 5: Tail Recursive - function call overhead
# Tests: AOT, WASM, daemon, Bun, Node.js, Porffor
# ─────────────────────────────────────────────────────────────────
if should_run tail_recursive; then
echo "─────────────────────────────────────────────────────────────────"
echo "5. Tail Recursive - function call overhead - ALL 6 RUNTIMES"
echo "─────────────────────────────────────────────────────────────────"

AOT_FILE="$ROOT_DIR/zig-out/bin/bench/tail_recursive.aot"
WASM_FILE="$ROOT_DIR/zig-out/bin/bench/tail_recursive.wasm"
JS_FILE="$SCRIPT_DIR/tail_recursive.js"

start_daemon "$AOT_FILE"

EDGEBOX_AOT_TIME=$(get_time "$EDGEBOX $AOT_FILE")
EDGEBOX_WASM_TIME=$(get_time "$WASM_RUNNER $WASM_FILE")
DAEMON_OUTPUT=$(curl -s http://localhost:$DAEMON_PORT/)
EDGEBOX_DAEMON_TIME=$(echo "$DAEMON_OUTPUT" | grep -oE '[0-9.]+ms' | grep -oE '[0-9.]+' | head -1)
if [ -z "$EDGEBOX_DAEMON_TIME" ]; then
    EDGEBOX_DAEMON_TIME="N/A"
fi
BUN_TIME=$(get_time "bun $JS_FILE")
NODE_TIME=$(get_time "node $JS_FILE")
PORFFOR_TIME=$(get_time "$PORFFOR $JS_FILE")

echo "  EdgeBox (AOT):    ${EDGEBOX_AOT_TIME}ms"
echo "  EdgeBox (WASM):   ${EDGEBOX_WASM_TIME}ms"
echo "  EdgeBox (daemon): ${EDGEBOX_DAEMON_TIME}ms"
echo "  Bun:              ${BUN_TIME}ms"
echo "  Node.js:          ${NODE_TIME}ms"
echo "  Porffor:          ${PORFFOR_TIME}ms"

cat > "$SCRIPT_DIR/results_tail_recursive.md" << EOF
| Runtime | Time |
|:---|---:|
| EdgeBox (AOT) | $(fmt_time "$EDGEBOX_AOT_TIME") |
| EdgeBox (WASM) | $(fmt_time "$EDGEBOX_WASM_TIME") |
| EdgeBox (daemon) | $(fmt_time "$EDGEBOX_DAEMON_TIME") |
| Bun | $(fmt_time "$BUN_TIME") |
| Node.js | $(fmt_time "$NODE_TIME") |
| Porffor | $(fmt_time "$PORFFOR_TIME") |
EOF

stop_daemon
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
echo ""
echo "Runtimes tested: EdgeBox (AOT, WASM, daemon), Bun, Node.js, Porffor"

# ─────────────────────────────────────────────────────────────────
# GITHUB ACTIONS SUMMARY
# ─────────────────────────────────────────────────────────────────
if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
    echo "Writing to GitHub Actions summary..."

    # Header with benchmark name if filtered
    if [ -n "$ONLY_BENCHMARK" ]; then
        echo "## 📊 Benchmark: $ONLY_BENCHMARK" >> "$GITHUB_STEP_SUMMARY"
    else
        echo "## 📊 EdgeBox Benchmark Results" >> "$GITHUB_STEP_SUMMARY"
    fi
    echo "" >> "$GITHUB_STEP_SUMMARY"

    # Append each result file that exists
    if [ -z "$ONLY_BENCHMARK" ] || [ "$ONLY_BENCHMARK" = "startup" ]; then
        if [ -f "$SCRIPT_DIR/results_startup.md" ]; then
            echo "### Startup (hello.js)" >> "$GITHUB_STEP_SUMMARY"
            cat "$SCRIPT_DIR/results_startup.md" >> "$GITHUB_STEP_SUMMARY"
            echo "" >> "$GITHUB_STEP_SUMMARY"
        fi
    fi

    if [ -z "$ONLY_BENCHMARK" ] || [ "$ONLY_BENCHMARK" = "memory" ]; then
        if [ -f "$SCRIPT_DIR/results_memory.md" ]; then
            echo "### Memory (allocation)" >> "$GITHUB_STEP_SUMMARY"
            cat "$SCRIPT_DIR/results_memory.md" >> "$GITHUB_STEP_SUMMARY"
            echo "" >> "$GITHUB_STEP_SUMMARY"
        fi
    fi

    if [ -z "$ONLY_BENCHMARK" ] || [ "$ONLY_BENCHMARK" = "fib" ]; then
        if [ -f "$SCRIPT_DIR/results_fib.md" ]; then
            echo "### Fibonacci (fib(35))" >> "$GITHUB_STEP_SUMMARY"
            cat "$SCRIPT_DIR/results_fib.md" >> "$GITHUB_STEP_SUMMARY"
            echo "" >> "$GITHUB_STEP_SUMMARY"
        fi
    fi

    if [ -z "$ONLY_BENCHMARK" ] || [ "$ONLY_BENCHMARK" = "loop" ]; then
        if [ -f "$SCRIPT_DIR/results_loop.md" ]; then
            echo "### Loop (array sum)" >> "$GITHUB_STEP_SUMMARY"
            cat "$SCRIPT_DIR/results_loop.md" >> "$GITHUB_STEP_SUMMARY"
            echo "" >> "$GITHUB_STEP_SUMMARY"
        fi
    fi

    if [ -z "$ONLY_BENCHMARK" ] || [ "$ONLY_BENCHMARK" = "tail_recursive" ]; then
        if [ -f "$SCRIPT_DIR/results_tail_recursive.md" ]; then
            echo "### Tail Recursive (function calls)" >> "$GITHUB_STEP_SUMMARY"
            cat "$SCRIPT_DIR/results_tail_recursive.md" >> "$GITHUB_STEP_SUMMARY"
            echo "" >> "$GITHUB_STEP_SUMMARY"
        fi
    fi

    echo "---" >> "$GITHUB_STEP_SUMMARY"
    echo "_Runtimes: EdgeBox (AOT, WASM, daemon), Bun, Node.js, Porffor_" >> "$GITHUB_STEP_SUMMARY"
fi
