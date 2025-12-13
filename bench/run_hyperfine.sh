#!/bin/bash
# EdgeBox Benchmark Suite
# Tests ALL 6 runtimes: EdgeBox (AOT), EdgeBox (WASM), EdgeBox (daemon), Bun, Node.js, Porffor
# Catches runtime failures and displays in summary, continues benchmarking

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"

echo "═══════════════════════════════════════════════════════════════"
echo "         EdgeBox Benchmark Suite - Fail-Fast Mode"
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
EDGEBOX_ROSETTA="$ROOT_DIR/zig-out/bin/edgebox-rosetta"

# Detect platform
PLATFORM=$(uname -s)
ARCH=$(uname -m)
WASM_RUNNER="$EDGEBOX"

if [ "$PLATFORM" = "Linux" ] && [ "$ARCH" = "x86_64" ]; then
    echo "Platform: Linux x86_64 - using native Fast JIT"
    if [ ! -f "$ROOT_DIR/vendor/wamr/product-mini/platforms/linux/build/libiwasm.a" ]; then
        echo "Building WAMR with Fast JIT..."
        (cd "$ROOT_DIR/vendor/wamr/product-mini/platforms/linux" && \
         mkdir -p build && cd build && \
         cmake .. -DWAMR_BUILD_FAST_JIT=1 -DWAMR_BUILD_INTERP=1 -DWAMR_BUILD_AOT=1 -DWAMR_BUILD_LIBC_WASI=1 -DWAMR_BUILD_SIMD=0 -DCMAKE_BUILD_TYPE=Release && \
         make -j$(nproc)) || { echo "ERROR: Failed to build WAMR"; exit 1; }
    fi
elif [ "$PLATFORM" = "Darwin" ] && [ "$ARCH" = "arm64" ]; then
    echo "Platform: macOS ARM64 - using edgebox-rosetta (Fast JIT via Rosetta 2)"
    if [ ! -x "$EDGEBOX_ROSETTA" ]; then
        echo "Building edgebox-rosetta..."
        (cd "$ROOT_DIR" && zig build runner-rosetta -Doptimize=ReleaseFast) || { echo "ERROR: Failed to build edgebox-rosetta"; exit 1; }
    fi
    WASM_RUNNER="$EDGEBOX_ROSETTA"
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

build_bench() {
    local name=$1
    local js_file="bench/$name.js"
    local wasm_file="$SCRIPT_DIR/$name.wasm"
    local aot_file="$SCRIPT_DIR/$name.aot"

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
    cd "$ROOT_DIR" && "$EDGEBOXC" build "$js_file" 2>&1 | grep -E '^\[build\]|\[warn\]' || true

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

build_bench hello
build_bench memory
build_bench fib
build_bench loop
build_bench tail_recursive

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
echo "─────────────────────────────────────────────────────────────────"
echo "1. Startup Time (hello.js) - ALL 6 RUNTIMES"
echo "─────────────────────────────────────────────────────────────────"

AOT_FILE="$SCRIPT_DIR/hello.aot"
WASM_FILE="$SCRIPT_DIR/hello.wasm"
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

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 2: Memory Usage (600k objects)
# Tests: AOT, WASM, daemon, Bun, Node.js, Porffor
# ─────────────────────────────────────────────────────────────────
echo "─────────────────────────────────────────────────────────────────"
echo "2. Memory Usage (600k objects) - ALL 6 RUNTIMES"
echo "─────────────────────────────────────────────────────────────────"

AOT_FILE="$SCRIPT_DIR/memory.aot"
WASM_FILE="$SCRIPT_DIR/memory.wasm"
JS_FILE="$SCRIPT_DIR/memory.js"

start_daemon "$AOT_FILE"

MEM_AOT=$(get_mem $EDGEBOX $AOT_FILE)
MEM_WASM=$(get_mem $WASM_RUNNER $WASM_FILE)
MEM_BUN=$(get_mem bun $JS_FILE)
MEM_NODE=$(get_mem node $JS_FILE)
MEM_PORFFOR=$(get_mem $PORFFOR $JS_FILE)

echo "  EdgeBox (AOT):    ${MEM_AOT}MB"
echo "  EdgeBox (WASM):   ${MEM_WASM}MB"
echo "  EdgeBox (daemon): (shared memory with daemon process)"
echo "  Bun:              ${MEM_BUN}MB"
echo "  Node.js:          ${MEM_NODE}MB"
echo "  Porffor:          ${MEM_PORFFOR}MB"

cat > "$SCRIPT_DIR/results_memory.md" << EOF
| Runtime | Memory |
|:---|---:|
| EdgeBox (AOT) | ${MEM_AOT}MB |
| EdgeBox (WASM) | ${MEM_WASM}MB |
| Bun | ${MEM_BUN}MB |
| Node.js | ${MEM_NODE}MB |
| Porffor | ${MEM_PORFFOR}MB |
EOF

stop_daemon
echo ""

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 3: Fibonacci fib(45) - frozen recursive
# Tests: AOT, WASM, daemon, Bun, Node.js, Porffor
# ─────────────────────────────────────────────────────────────────
echo "─────────────────────────────────────────────────────────────────"
echo "3. Fibonacci fib(45) - frozen recursive - ALL 6 RUNTIMES"
echo "─────────────────────────────────────────────────────────────────"

AOT_FILE="$SCRIPT_DIR/fib.aot"
WASM_FILE="$SCRIPT_DIR/fib.wasm"
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

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 4: Loop (array sum) - frozen array iteration
# Tests: AOT, WASM, daemon, Bun, Node.js, Porffor
# ─────────────────────────────────────────────────────────────────
echo "─────────────────────────────────────────────────────────────────"
echo "4. Loop (array sum) - frozen array iteration - ALL 6 RUNTIMES"
echo "─────────────────────────────────────────────────────────────────"

AOT_FILE="$SCRIPT_DIR/loop.aot"
WASM_FILE="$SCRIPT_DIR/loop.wasm"
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

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 5: Tail Recursive - function call overhead
# Tests: AOT, WASM, daemon, Bun, Node.js, Porffor
# ─────────────────────────────────────────────────────────────────
echo "─────────────────────────────────────────────────────────────────"
echo "5. Tail Recursive - function call overhead - ALL 6 RUNTIMES"
echo "─────────────────────────────────────────────────────────────────"

AOT_FILE="$SCRIPT_DIR/tail_recursive.aot"
WASM_FILE="$SCRIPT_DIR/tail_recursive.wasm"
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
