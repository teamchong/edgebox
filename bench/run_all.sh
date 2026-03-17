#!/bin/bash
# Run ALL AOT+JIT benchmarks and generate a summary
set -e

echo "=== EdgeBox JIT+AOT Benchmark Suite ==="
echo "Date: $(date)"
echo "Node: $(node --version)"
echo ""

FILES=(
    bench/fib.js
    bench/compute_heavy.js
    bench/loop.js
    bench/loop_zerocopy.js
    bench/data_processing.js
    bench/f64_array.js
    bench/math_intrinsics.js
    bench/sieve.js
    bench/bitops.js
    bench/sha256.js
    bench/pako_kernels.js
    bench/mandelbrot.js
    bench/gaussian_blur.js
    bench/sorting.js
)

TOTAL_AOT=0
TOTAL_NODE=0
WINS=0
LOSSES=0
TIES=0

for FILE in "${FILES[@]}"; do
    [ -f "$FILE" ] || continue
    echo "=== $(basename $FILE .js) ==="
    OUTPUT=$(bash bench/compare_aot_jit.sh "$FILE" 2>/dev/null)
    echo "$OUTPUT" | grep -E '^\|' | grep -v '^|--'
    echo ""

    # Accumulate totals for geomean
    while IFS= read -r line; do
        aot_ms=$(echo "$line" | grep -oP '\|\s*\K[0-9]+(?=ms\s*\|)' | head -1)
        node_ms=$(echo "$line" | grep -oP '\|\s*\K[0-9]+(?=ms\s*\|)' | tail -1)
        [ -z "$aot_ms" ] && continue
        [ -z "$node_ms" ] && continue
        [ "$aot_ms" -eq 0 ] && continue
        [ "$node_ms" -eq 0 ] && continue
        TOTAL_AOT=$((TOTAL_AOT + aot_ms))
        TOTAL_NODE=$((TOTAL_NODE + node_ms))
        if [ "$aot_ms" -lt "$node_ms" ]; then
            WINS=$((WINS + 1))
        elif [ "$aot_ms" -gt "$node_ms" ]; then
            LOSSES=$((LOSSES + 1))
        else
            TIES=$((TIES + 1))
        fi
    done <<< "$(echo "$OUTPUT" | grep -E '^\|' | grep -v '^|--' | grep -v 'Benchmark')"
done

echo "=== Summary ==="
TOTAL=$((WINS + LOSSES + TIES))
if [ "$TOTAL_AOT" -gt 0 ]; then
    RATIO=$(echo "scale=2; $TOTAL_NODE / $TOTAL_AOT" | bc)
    echo "Overall: ${RATIO}x (total ${TOTAL_AOT}ms AOT+JIT vs ${TOTAL_NODE}ms Node.js)"
fi
echo "Wins: $WINS | Ties: $TIES | Losses: $LOSSES (of $TOTAL benchmarks)"
