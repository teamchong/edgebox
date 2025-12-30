#!/bin/bash
# HTTP Server Benchmark - EdgeBox vs Bun vs Node.js
# Tests throughput using wrk or ab

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"

echo "═══════════════════════════════════════════════════════════════"
echo "              HTTP Server Benchmark"
echo "═══════════════════════════════════════════════════════════════"
echo ""

# Check prerequisites
for cmd in bun node curl; do
    if ! command -v $cmd &> /dev/null; then
        echo "ERROR: Required command '$cmd' not found."
        exit 1
    fi
done

# Check for wrk or ab
LOAD_TOOL=""
if command -v wrk &> /dev/null; then
    LOAD_TOOL="wrk"
elif command -v ab &> /dev/null; then
    LOAD_TOOL="ab"
else
    echo "ERROR: Neither 'wrk' nor 'ab' found. Install one of them."
    echo "  macOS: brew install wrk"
    echo "  Linux: apt install wrk OR apt install apache2-utils"
    exit 1
fi

echo "Using load testing tool: $LOAD_TOOL"

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

# Configuration
PORT_EDGEBOX=8889
PORT_BUN=8890
PORT_NODE=8891
DURATION=10
THREADS=4
CONNECTIONS=100

# Results storage
RESULTS_FILE="$SCRIPT_DIR/results_http.md"

# Cleanup function
cleanup() {
    echo "Cleaning up..."
    # Kill any servers we started
    pkill -f "edgebox.*http_native" 2>/dev/null || true
    pkill -f "bun.*bun_server" 2>/dev/null || true
    pkill -f "node.*node_server" 2>/dev/null || true
    sleep 1
}
trap cleanup EXIT

# Wait for server to be ready
wait_for_server() {
    local port=$1
    local max_attempts=30
    local attempt=0

    while [ $attempt -lt $max_attempts ]; do
        if curl -s "http://localhost:$port/" > /dev/null 2>&1; then
            return 0
        fi
        sleep 0.2
        attempt=$((attempt + 1))
    done
    return 1
}

# Run load test and extract requests/sec
run_load_test() {
    local url=$1
    local name=$2

    if [ "$LOAD_TOOL" = "wrk" ]; then
        # wrk output format: Requests/sec: 12345.67
        result=$(wrk -t$THREADS -c$CONNECTIONS -d${DURATION}s "$url" 2>&1)
        rps=$(echo "$result" | grep "Requests/sec" | awk '{print $2}')
        latency=$(echo "$result" | grep "Latency" | awk '{print $2}')
        echo "[$name] Requests/sec: $rps, Avg Latency: $latency"
        echo "$rps"
    else
        # ab output format: Requests per second: 12345.67 [#/sec]
        result=$(ab -t $DURATION -c $CONNECTIONS "$url" 2>&1)
        rps=$(echo "$result" | grep "Requests per second" | awk '{print $4}')
        latency=$(echo "$result" | grep "Time per request" | head -1 | awk '{print $4}')
        echo "[$name] Requests/sec: $rps, Avg Latency: ${latency}ms"
        echo "$rps"
    fi
}

# Build function - run from ROOT_DIR to get predictable paths
build_http_bench() {
    local js_rel="bench/http_native/index.js"
    local aot_file="$ROOT_DIR/zig-out/bin/$js_rel/index.aot"

    if [ ! -f "$ROOT_DIR/$js_rel" ]; then
        echo "ERROR: HTTP benchmark source not found: $ROOT_DIR/$js_rel"
        return 1
    fi

    # Skip build if AOT already exists and is newer than source
    if [ -f "$aot_file" ] && [ "$aot_file" -nt "$ROOT_DIR/$js_rel" ]; then
        echo "  http_native: using cached build"
        return 0
    fi

    echo "  Building http_native..."
    cd "$ROOT_DIR" && "$EDGEBOXC" "$js_rel" 2>&1 | tail -10

    if [ ! -f "$aot_file" ]; then
        echo "ERROR: Failed to build http_native (expected $aot_file)"
        return 1
    fi
}

# Create Bun server file
BUN_SERVER_FILE="/tmp/bun_server.js"
cat > "$BUN_SERVER_FILE" << 'EOF'
Bun.serve({
  port: 8890,
  fetch(req) {
    return new Response("Hello, World!");
  },
});
EOF

# Create Node.js server file
NODE_SERVER_FILE="/tmp/node_server.js"
cat > "$NODE_SERVER_FILE" << 'EOF'
const http = require('http');
const server = http.createServer((req, res) => {
  res.writeHead(200, { 'Content-Type': 'text/plain' });
  res.end('Hello, World!');
});
server.listen(8891);
EOF

echo ""
echo "Building EdgeBox HTTP benchmark..."
build_http_bench

HTTP_AOT="$ROOT_DIR/zig-out/bin/bench/http_native/index.js/index.aot"

echo ""
echo "─────────────────────────────────────────────────────────────────"
echo "Test 1: EdgeBox Native HTTP Server"
echo "─────────────────────────────────────────────────────────────────"

# Start EdgeBox server
echo "Starting EdgeBox server on port $PORT_EDGEBOX..."
"$EDGEBOX" "$HTTP_AOT" &
EDGEBOX_PID=$!

if ! wait_for_server $PORT_EDGEBOX; then
    echo "ERROR: EdgeBox server failed to start"
    kill $EDGEBOX_PID 2>/dev/null
    exit 1
fi

echo "EdgeBox server ready. Running load test..."
EDGEBOX_RPS=$(run_load_test "http://localhost:$PORT_EDGEBOX/" "EdgeBox")

# Stop EdgeBox
kill $EDGEBOX_PID 2>/dev/null
wait $EDGEBOX_PID 2>/dev/null
sleep 1

echo ""
echo "─────────────────────────────────────────────────────────────────"
echo "Test 2: Bun HTTP Server"
echo "─────────────────────────────────────────────────────────────────"

# Start Bun server
echo "Starting Bun server on port $PORT_BUN..."
bun run "$BUN_SERVER_FILE" &
BUN_PID=$!

if ! wait_for_server $PORT_BUN; then
    echo "ERROR: Bun server failed to start"
    kill $BUN_PID 2>/dev/null
    exit 1
fi

echo "Bun server ready. Running load test..."
BUN_RPS=$(run_load_test "http://localhost:$PORT_BUN/" "Bun")

# Stop Bun
kill $BUN_PID 2>/dev/null
wait $BUN_PID 2>/dev/null
sleep 1

echo ""
echo "─────────────────────────────────────────────────────────────────"
echo "Test 3: Node.js HTTP Server"
echo "─────────────────────────────────────────────────────────────────"

# Start Node.js server
echo "Starting Node.js server on port $PORT_NODE..."
node "$NODE_SERVER_FILE" &
NODE_PID=$!

if ! wait_for_server $PORT_NODE; then
    echo "ERROR: Node.js server failed to start"
    kill $NODE_PID 2>/dev/null
    exit 1
fi

echo "Node.js server ready. Running load test..."
NODE_RPS=$(run_load_test "http://localhost:$PORT_NODE/" "Node.js")

# Stop Node.js
kill $NODE_PID 2>/dev/null
wait $NODE_PID 2>/dev/null

echo ""
echo "═══════════════════════════════════════════════════════════════"
echo "                       Results Summary"
echo "═══════════════════════════════════════════════════════════════"
echo ""

# Calculate speedups
if [ -n "$EDGEBOX_RPS" ] && [ -n "$BUN_RPS" ] && [ -n "$NODE_RPS" ]; then
    # Extract numeric values (remove any non-numeric suffixes)
    EDGEBOX_NUM=$(echo "$EDGEBOX_RPS" | sed 's/[^0-9.]//g')
    BUN_NUM=$(echo "$BUN_RPS" | sed 's/[^0-9.]//g')
    NODE_NUM=$(echo "$NODE_RPS" | sed 's/[^0-9.]//g')

    if [ -n "$EDGEBOX_NUM" ] && [ -n "$BUN_NUM" ] && [ -n "$NODE_NUM" ] && [ "$BUN_NUM" != "0" ] && [ "$NODE_NUM" != "0" ]; then
        SPEEDUP_BUN=$(echo "scale=2; $EDGEBOX_NUM / $BUN_NUM" | bc)
        SPEEDUP_NODE=$(echo "scale=2; $EDGEBOX_NUM / $NODE_NUM" | bc)
        echo "EdgeBox:  $EDGEBOX_RPS req/sec"
        echo "Bun:      $BUN_RPS req/sec"
        echo "Node.js:  $NODE_RPS req/sec"
        echo ""
        echo "EdgeBox vs Bun:  ${SPEEDUP_BUN}x"
        echo "EdgeBox vs Node: ${SPEEDUP_NODE}x"

        # Write results
        cat > "$RESULTS_FILE" << EOF
| Runtime | Requests/sec |
|---------|-------------|
| EdgeBox | $EDGEBOX_RPS |
| Bun | $BUN_RPS |
| Node.js | $NODE_RPS |

**EdgeBox vs Bun: ${SPEEDUP_BUN}x | EdgeBox vs Node: ${SPEEDUP_NODE}x**
EOF
        echo ""
        echo "Results saved to: $RESULTS_FILE"
    else
        echo "ERROR: Could not parse benchmark results"
        echo "EdgeBox raw: $EDGEBOX_RPS"
        echo "Bun raw: $BUN_RPS"
        echo "Node.js raw: $NODE_RPS"
        exit 1
    fi
else
    echo "ERROR: Missing benchmark results"
    exit 1
fi
