#!/bin/bash
# IO Benchmark - EdgeBox vs Bun
# Tests file read/write performance

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"

echo "═══════════════════════════════════════════════════════════════"
echo "                  IO Benchmark (File Read/Write)"
echo "═══════════════════════════════════════════════════════════════"
echo ""

# Check prerequisites
if ! command -v bun &> /dev/null; then
    echo "ERROR: bun not found."
    exit 1
fi

EDGEBOX="$ROOT_DIR/zig-out/bin/edgebox"
EDGEBOXC="$ROOT_DIR/zig-out/bin/edgeboxc"

if [ ! -f "$EDGEBOX" ]; then
    echo "ERROR: EdgeBox binary not found at: $EDGEBOX"
    echo "Run: cd $ROOT_DIR && zig build cli"
    exit 1
fi

if [ ! -f "$EDGEBOXC" ]; then
    echo "ERROR: EdgeBoxC compiler not found at: $EDGEBOXC"
    echo "Run: cd $ROOT_DIR && zig build cli"
    exit 1
fi

# Suppress debug messages
export EDGEBOX_QUIET=1

# Build function - run from ROOT_DIR to get predictable paths
build_bench() {
    local name=$1
    local js_rel="bench/io_bench/${name}.js"
    local aot_file="$ROOT_DIR/zig-out/bin/$js_rel/${name}.aot"

    if [ ! -f "$ROOT_DIR/$js_rel" ]; then
        echo "ERROR: Benchmark source not found: $ROOT_DIR/$js_rel"
        return 1
    fi

    # Skip build if AOT already exists and is newer than source
    if [ -f "$aot_file" ] && [ "$aot_file" -nt "$ROOT_DIR/$js_rel" ]; then
        echo "  $name: using cached build"
        return 0
    fi

    echo "  Building $name..."
    cd "$ROOT_DIR" && "$EDGEBOXC" "$js_rel" 2>&1 | tail -10

    if [ ! -f "$aot_file" ]; then
        echo "ERROR: Failed to build $name (expected $aot_file)"
        return 1
    fi
}

# Create test file for read benchmark
TEST_FILE="/tmp/io_bench_test.txt"
echo "Creating test file ($TEST_FILE)..."
dd if=/dev/urandom bs=1024 count=100 2>/dev/null | base64 > "$TEST_FILE"
echo "Test file size: $(wc -c < "$TEST_FILE") bytes"

# Results storage
RESULTS_FILE="$SCRIPT_DIR/results_io.md"

echo ""
echo "Building EdgeBox benchmarks..."
build_bench "read_bench"
build_bench "write_bench"

echo ""
echo "─────────────────────────────────────────────────────────────────"
echo "Test 1: File Read Performance (1000 iterations)"
echo "─────────────────────────────────────────────────────────────────"

# EdgeBox read benchmark
echo "Running EdgeBox read benchmark..."
READ_AOT="$ROOT_DIR/zig-out/bin/bench/io_bench/read_bench.js/read_bench.aot"
EDGEBOX_READ=$("$EDGEBOX" "$READ_AOT" 2>&1 | grep "read_ops_per_sec" | cut -d':' -f2)
echo "EdgeBox: $EDGEBOX_READ ops/sec"

# Bun read benchmark
echo "Running Bun read benchmark..."
BUN_READ=$(bun run "$SCRIPT_DIR/io_bench/read_bench_bun.js" 2>&1 | grep "read_ops_per_sec" | cut -d':' -f2)
echo "Bun: $BUN_READ ops/sec"

echo ""
echo "─────────────────────────────────────────────────────────────────"
echo "Test 2: File Write Performance (1000 iterations)"
echo "─────────────────────────────────────────────────────────────────"

# EdgeBox write benchmark
echo "Running EdgeBox write benchmark..."
WRITE_AOT="$ROOT_DIR/zig-out/bin/bench/io_bench/write_bench.js/write_bench.aot"
EDGEBOX_WRITE=$("$EDGEBOX" "$WRITE_AOT" 2>&1 | grep "write_ops_per_sec" | cut -d':' -f2)
echo "EdgeBox: $EDGEBOX_WRITE ops/sec"

# Bun write benchmark
echo "Running Bun write benchmark..."
BUN_WRITE=$(bun run "$SCRIPT_DIR/io_bench/write_bench_bun.js" 2>&1 | grep "write_ops_per_sec" | cut -d':' -f2)
echo "Bun: $BUN_WRITE ops/sec"

echo ""
echo "═══════════════════════════════════════════════════════════════"
echo "                       Results Summary"
echo "═══════════════════════════════════════════════════════════════"
echo ""

# Calculate speedups
READ_SPEEDUP="N/A"
WRITE_SPEEDUP="N/A"

if [ -n "$EDGEBOX_READ" ] && [ -n "$BUN_READ" ] && [ "$BUN_READ" != "0" ]; then
    READ_SPEEDUP=$(echo "scale=2; $EDGEBOX_READ / $BUN_READ" | bc)
fi

if [ -n "$EDGEBOX_WRITE" ] && [ -n "$BUN_WRITE" ] && [ "$BUN_WRITE" != "0" ]; then
    WRITE_SPEEDUP=$(echo "scale=2; $EDGEBOX_WRITE / $BUN_WRITE" | bc)
fi

echo "File Read:"
echo "  EdgeBox: $EDGEBOX_READ ops/sec"
echo "  Bun:     $BUN_READ ops/sec"
echo "  Speedup: ${READ_SPEEDUP}x"
echo ""
echo "File Write:"
echo "  EdgeBox: $EDGEBOX_WRITE ops/sec"
echo "  Bun:     $BUN_WRITE ops/sec"
echo "  Speedup: ${WRITE_SPEEDUP}x"

# Write results
cat > "$RESULTS_FILE" << EOF
### File Read (1000 iterations)
| Runtime | Ops/sec |
|---------|---------|
| EdgeBox | $EDGEBOX_READ |
| Bun | $BUN_READ |

**Read Speedup: ${READ_SPEEDUP}x**

### File Write (1000 iterations)
| Runtime | Ops/sec |
|---------|---------|
| EdgeBox | $EDGEBOX_WRITE |
| Bun | $BUN_WRITE |

**Write Speedup: ${WRITE_SPEEDUP}x**
EOF

echo ""
echo "Results saved to: $RESULTS_FILE"

# Cleanup
rm -f "$TEST_FILE" /tmp/io_bench_write_test.txt
