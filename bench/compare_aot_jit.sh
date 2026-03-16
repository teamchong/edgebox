#!/bin/bash
# AOT+JIT vs Node.js Benchmark Comparison
# Compiles JS to standalone WASM, runs AOT+JIT (WASM in V8) vs pure Node.js
#
# Usage: ./bench/compare_aot_jit.sh [bench/compute_heavy.js]

set -e

EDGEBOXC="./zig-out/bin/edgeboxc"
NODE="node"
FILE="${1:-bench/compute_heavy.js}"
RUNS=3  # Run each N times, take best

if [ ! -f "$FILE" ]; then
    echo "ERROR: $FILE not found"
    exit 1
fi

basename=$(basename "${FILE%.js}")
outdir="zig-out/bin/$FILE"
worker="$outdir/${basename}-worker.mjs"
wasm="$outdir/${basename}-standalone.wasm"

# Step 1: Compile
echo "--- Compiling $FILE ---"
rm -rf "$outdir"
$EDGEBOXC --wasm-only "$FILE" 2>&1 | grep -E 'Standalone WASM:|Cross-call:|Numeric'

if [ ! -f "$worker" ]; then
    echo "ERROR: Worker not generated: $worker"
    exit 1
fi
if [ ! -f "$wasm" ]; then
    echo "ERROR: WASM not generated: $wasm"
    exit 1
fi

wasm_size=$(stat -c%s "$wasm" 2>/dev/null || stat -f%z "$wasm")
echo "WASM size: $wasm_size bytes"
echo ""

# Step 2: Run benchmarks
echo "--- Running benchmarks ($RUNS runs each, best of) ---"
echo ""

# Extract benchmark lines and parse "name: Nms"
extract_benchmarks() {
    local output="$1"
    echo "$output" | grep -E '^[a-zA-Z].*: [0-9]+ms' || true
}

# Run N times, collect all output
run_best() {
    local cmd="$1"
    local best_output=""
    local best_total=999999999

    for run in $(seq 1 $RUNS); do
        local output
        output=$(eval "$cmd" 2>&1) || true
        local total=0
        while IFS= read -r line; do
            local ms
            ms=$(echo "$line" | grep -oP ': \K[0-9]+(?=ms)')
            if [ -n "$ms" ]; then
                total=$((total + ms))
            fi
        done <<< "$(extract_benchmarks "$output")"
        if [ "$total" -lt "$best_total" ] && [ "$total" -gt 0 ]; then
            best_total=$total
            best_output="$output"
        fi
    done
    echo "$best_output"
}

echo "Running Node.js baseline..."
node_output=$(run_best "$NODE $FILE")
echo "Running AOT+JIT (WASM in V8)..."
aot_output=$(run_best "$NODE $worker")

# Step 3: Display comparison table
echo ""
echo "| Benchmark | AOT+JIT | Node.js | Ratio |"
echo "|-----------|---------|---------|-------|"

while IFS= read -r aot_line; do
    name=$(echo "$aot_line" | sed 's/: [0-9]*ms.*//')
    aot_ms=$(echo "$aot_line" | grep -oP ': \K[0-9]+(?=ms)')
    [ -z "$aot_ms" ] && continue

    # Find matching benchmark in node output
    node_ms=$(echo "$node_output" | grep "^$name:" | grep -oP ': \K[0-9]+(?=ms)' | head -1)
    [ -z "$node_ms" ] && node_ms="?"

    if [ "$node_ms" != "?" ] && [ "$aot_ms" -gt 0 ]; then
        if [ "$aot_ms" -lt "$node_ms" ]; then
            ratio=$(echo "scale=1; $node_ms / $aot_ms" | bc)
            label="${ratio}x faster"
        elif [ "$aot_ms" -gt "$node_ms" ]; then
            ratio=$(echo "scale=1; $aot_ms / $node_ms" | bc)
            label="${ratio}x slower"
        else
            label="same"
        fi
    else
        label="?"
    fi

    printf "| %-20s | %7sms | %7sms | %-12s |\n" "$name" "$aot_ms" "$node_ms" "$label"
done <<< "$(extract_benchmarks "$aot_output")"

echo ""
echo "WASM: $wasm_size bytes | Node: $(node --version)"
