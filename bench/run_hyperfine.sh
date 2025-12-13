#!/bin/bash
# EdgeBox Benchmark Suite
# Compares runtimes: EdgeBox (AOT, WASM, daemon), Bun, Node.js, Porffor

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"

if [ ! -f "$ROOT_DIR/build.zig" ]; then
    echo "ERROR: Cannot find EdgeBox repo. Expected build.zig at: $ROOT_DIR/build.zig"
    exit 1
fi

EDGEBOX="$ROOT_DIR/zig-out/bin/edgebox"
EDGEBOXC="$ROOT_DIR/zig-out/bin/edgeboxc"
EDGEBOX_ROSETTA="$ROOT_DIR/zig-out/bin/edgebox-rosetta"

# Check for hyperfine
if ! command -v hyperfine &> /dev/null; then
    echo "Installing hyperfine..."
    brew install hyperfine
fi

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
    if [ -x "$EDGEBOX_ROSETTA" ]; then
        WASM_RUNNER="$EDGEBOX_ROSETTA"
    else
        echo "Building edgebox-rosetta..."
        (cd "$ROOT_DIR" && zig build runner-rosetta -Doptimize=ReleaseFast) || { echo "ERROR: Failed to build"; exit 1; }
        WASM_RUNNER="$EDGEBOX_ROSETTA"
    fi
fi

# Build CLI if needed
if [ ! -x "$EDGEBOX" ]; then
    echo "Building edgebox CLI..."
    cd "$ROOT_DIR" && zig build cli -Doptimize=ReleaseFast
fi

# Build benchmark artifacts
build_bench() {
    local name=$1
    local js_file="$SCRIPT_DIR/$name.js"
    local wasm_file="$SCRIPT_DIR/$name.wasm"
    local aot_file="$SCRIPT_DIR/$name.aot"

    if [ -f "$js_file" ]; then
        echo "Building $name..."
        rm -f "$wasm_file" "$aot_file" "$ROOT_DIR/$name.wasm" "$ROOT_DIR/$name.aot" 2>/dev/null
        cd "$ROOT_DIR" && "$EDGEBOXC" build "$js_file" 2>&1 | grep -v "^\[" | grep -v "^  Atom" || true
        [ -f "$ROOT_DIR/$name.wasm" ] && mv "$ROOT_DIR/$name.wasm" "$wasm_file"
        [ -f "$ROOT_DIR/$name.aot" ] && mv "$ROOT_DIR/$name.aot" "$aot_file"
        # Files now in zig-out/ which is auto-cleaned by build system
    fi
}

build_bench hello
build_bench memory
build_bench fib
build_bench loop
build_bench tail_recursive

# Porffor detection
PORFFOR=""
if [ -x "$HOME/.local/share/mise/installs/node/20.18.0/lib/node_modules/porffor/porf" ]; then
    PORFFOR="$HOME/.local/share/mise/installs/node/20.18.0/lib/node_modules/porffor/porf"
elif command -v porf &> /dev/null; then
    PORFFOR="porf"
fi

# Build Porffor native binaries if available
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
    build_porffor_native memory
    build_porffor_native fib
    build_porffor_native loop
    build_porffor_native tail_recursive
fi

echo ""
echo "═══════════════════════════════════════════════════════════════"
echo "                    EdgeBox Benchmark Suite"
echo "═══════════════════════════════════════════════════════════════"
echo ""

# Benchmark parameters
BENCH_RUNS=5
BENCH_WARMUP=1

# Daemon setup
EDGEBOXD="$ROOT_DIR/zig-out/bin/edgeboxd"
DAEMON_PORT=18080
DAEMON_PID=""

start_daemon() {
    local dylib_file=$1
    if [ -x "$EDGEBOXD" ] && [ -f "$dylib_file" ]; then
        "$EDGEBOXD" "$dylib_file" --port=$DAEMON_PORT >/dev/null 2>&1 &
        DAEMON_PID=$!
        sleep 0.5
        if printf "GET / HTTP/1.0\r\n\r\n" | nc -w1 localhost $DAEMON_PORT >/dev/null 2>&1; then
            return 0
        fi
    fi
    return 1
}

stop_daemon() {
    if [ -n "$DAEMON_PID" ]; then
        kill $DAEMON_PID 2>/dev/null || true
        wait $DAEMON_PID 2>/dev/null || true
        DAEMON_PID=""
        sleep 0.2
    fi
}

run_benchmark() {
    local name=$1
    local runs=${2:-$BENCH_RUNS}
    local warmup=${3:-$BENCH_WARMUP}
    local aot_file=$4
    local js_file=$5
    local output_file=$6
    local wasm_file="$SCRIPT_DIR/$name.wasm"
    local porffor_native="$SCRIPT_DIR/${name}_porffor"

    start_daemon "$aot_file"
    local daemon_available=$?

    local cmd="hyperfine --warmup $warmup --runs $runs"
    cmd+=" -n 'EdgeBox (AOT)' '$EDGEBOX $aot_file 2>/dev/null'"
    [ -f "$wasm_file" ] && cmd+=" -n 'EdgeBox (WASM)' '$WASM_RUNNER $wasm_file 2>/dev/null'"
    [ $daemon_available -eq 0 ] && cmd+=" -n 'EdgeBox (daemon)' 'curl -s http://localhost:$DAEMON_PORT/'"
    cmd+=" -n 'Bun' 'bun $js_file'"
    cmd+=" -n 'Node.js' 'node $js_file'"
    [ -x "$porffor_native" ] && cmd+=" -n 'Porffor (native)' '$porffor_native'"
    cmd+=" --export-markdown '$output_file'"

    eval $cmd || echo "WARNING: hyperfine failed for $name"
    stop_daemon
}

# Helper to extract timing from benchmark output
get_time() {
    local output=$(eval "$1" 2>/dev/null | grep -E '^[0-9]+ \(' | head -1)
    echo "$output" | grep -oE '\([0-9.]+ms' | grep -oE '[0-9.]+' | head -1
}

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 1: Cold Start
# ─────────────────────────────────────────────────────────────────
echo "─────────────────────────────────────────────────────────────────"
echo "1. Cold Start (hello.js)"
echo "─────────────────────────────────────────────────────────────────"

get_size() { ls -lh "$1" 2>/dev/null | awk '{print $5}' || echo "N/A"; }
echo "  AOT: $(get_size $SCRIPT_DIR/hello.aot), WASM: $(get_size $SCRIPT_DIR/hello.wasm), JS: $(get_size $SCRIPT_DIR/hello.js)"
echo ""

run_benchmark "hello" $BENCH_RUNS 0 "$SCRIPT_DIR/hello.aot" "$SCRIPT_DIR/hello.js" "$SCRIPT_DIR/results_cold_start.md"
echo ""

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 2: Memory Usage
# ─────────────────────────────────────────────────────────────────
echo "─────────────────────────────────────────────────────────────────"
echo "2. Memory Usage (600k objects)"
echo "─────────────────────────────────────────────────────────────────"

get_mem() {
    local output=$(/usr/bin/time -l "$@" 2>&1)
    local bytes=$(echo "$output" | grep "maximum resident set size" | awk 'NF{print $1}')
    if [ -z "$bytes" ]; then
        echo "ERROR: Failed to parse memory output" >&2
        echo "$output" >&2
        exit 1
    fi
    echo "scale=1; $bytes / 1024 / 1024" | bc
}

echo "  EdgeBox (AOT): $(get_mem $EDGEBOX $SCRIPT_DIR/memory.aot 2>/dev/null)MB"
[ -f "$SCRIPT_DIR/memory.wasm" ] && echo "  EdgeBox (WASM): $(get_mem $WASM_RUNNER $SCRIPT_DIR/memory.wasm 2>/dev/null)MB"
echo "  Bun: $(get_mem bun $SCRIPT_DIR/memory.js)MB"
echo "  Node.js: $(get_mem node $SCRIPT_DIR/memory.js)MB"
[ -x "$SCRIPT_DIR/memory_porffor" ] && echo "  Porffor (native): $(get_mem $SCRIPT_DIR/memory_porffor)MB"
echo ""

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 3: Fibonacci (frozen recursive)
# ─────────────────────────────────────────────────────────────────
echo "─────────────────────────────────────────────────────────────────"
echo "3. Fibonacci fib(45) - frozen recursive"
echo "─────────────────────────────────────────────────────────────────"

EDGEBOX_FIB=$(get_time "$EDGEBOX $SCRIPT_DIR/fib.aot")
echo "  EdgeBox (AOT): ${EDGEBOX_FIB}ms"
[ -f "$SCRIPT_DIR/fib.wasm" ] && echo "  EdgeBox (WASM): $(get_time "$WASM_RUNNER $SCRIPT_DIR/fib.wasm")ms"
echo "  Bun: $(get_time "bun $SCRIPT_DIR/fib.js")ms"
echo "  Node.js: $(get_time "node $SCRIPT_DIR/fib.js")ms"
[ -n "$PORFFOR" ] && echo "  Porffor: $(get_time "$PORFFOR $SCRIPT_DIR/fib.js")ms"

cat > "$SCRIPT_DIR/results_fib.md" << EOF
| Runtime | Time | Relative |
|:---|---:|---:|
| EdgeBox (AOT) | ${EDGEBOX_FIB}ms | 1.00 |
EOF
echo ""

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 4: Loop (frozen array iteration)
# ─────────────────────────────────────────────────────────────────
echo "─────────────────────────────────────────────────────────────────"
echo "4. Loop (array sum) - frozen array iteration"
echo "─────────────────────────────────────────────────────────────────"

EDGEBOX_LOOP=$(get_time "$EDGEBOX $SCRIPT_DIR/loop.aot")
echo "  EdgeBox (AOT): ${EDGEBOX_LOOP}ms"
[ -f "$SCRIPT_DIR/loop.wasm" ] && echo "  EdgeBox (WASM): $(get_time "$WASM_RUNNER $SCRIPT_DIR/loop.wasm")ms"
echo "  Bun: $(get_time "bun $SCRIPT_DIR/loop.js")ms"
echo "  Node.js: $(get_time "node $SCRIPT_DIR/loop.js")ms"
[ -n "$PORFFOR" ] && echo "  Porffor: $(get_time "$PORFFOR $SCRIPT_DIR/loop.js")ms"

cat > "$SCRIPT_DIR/results_loop.md" << EOF
| Runtime | Time | Relative |
|:---|---:|---:|
| EdgeBox (AOT) | ${EDGEBOX_LOOP}ms | 1.00 |
EOF
echo ""

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 5: Tail Recursive (function call overhead)
# ─────────────────────────────────────────────────────────────────
echo "─────────────────────────────────────────────────────────────────"
echo "5. Tail Recursive - function call overhead (NOT frozen yet)"
echo "─────────────────────────────────────────────────────────────────"

EDGEBOX_TAILREC=$(get_time "$EDGEBOX $SCRIPT_DIR/tail_recursive.aot")
echo "  EdgeBox (AOT): ${EDGEBOX_TAILREC}ms"
[ -f "$SCRIPT_DIR/tail_recursive.wasm" ] && echo "  EdgeBox (WASM): $(get_time "$WASM_RUNNER $SCRIPT_DIR/tail_recursive.wasm")ms"
echo "  Bun: $(get_time "bun $SCRIPT_DIR/tail_recursive.js")ms"
echo "  Node.js: $(get_time "node $SCRIPT_DIR/tail_recursive.js")ms"
[ -n "$PORFFOR" ] && echo "  Porffor: $(get_time "$PORFFOR $SCRIPT_DIR/tail_recursive.js")ms"

cat > "$SCRIPT_DIR/results_tail_recursive.md" << EOF
| Runtime | Time | Relative |
|:---|---:|---:|
| EdgeBox (AOT) | ${EDGEBOX_TAILREC}ms | 1.00 |
EOF
echo ""

# ─────────────────────────────────────────────────────────────────
# BENCHMARK 6: Daemon Warm Pod
# ─────────────────────────────────────────────────────────────────
echo "─────────────────────────────────────────────────────────────────"
echo "6. Daemon Warm Pod"
echo "─────────────────────────────────────────────────────────────────"

if [ -x "$EDGEBOXD" ]; then
    "$EDGEBOXD" "$SCRIPT_DIR/hello.aot" --pool-size=8 --port=$DAEMON_PORT >/dev/null 2>&1 &
    DAEMON_PID=$!
    sleep 1

    if printf "GET / HTTP/1.0\r\n\r\n" | nc -w1 localhost $DAEMON_PORT >/dev/null 2>&1; then
        curl -s http://localhost:$DAEMON_PORT/ >/dev/null 2>&1  # warmup
        hyperfine --warmup 3 --runs 10 \
            -n 'EdgeBox (daemon)' "curl -s http://localhost:$DAEMON_PORT/" \
            --export-markdown "$SCRIPT_DIR/results_daemon_warm.md"
        stop_daemon
    else
        echo "SKIP: Could not start daemon"
    fi
else
    echo "SKIP: edgeboxd not found"
fi

echo ""
echo "═══════════════════════════════════════════════════════════════"
echo "                    Benchmark Complete!"
echo "═══════════════════════════════════════════════════════════════"
