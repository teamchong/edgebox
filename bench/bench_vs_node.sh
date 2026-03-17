#!/bin/bash
# EdgeBox vs Node.js Benchmark Runner
# Compiles each benchmark with edgebox, runs in both EdgeBox and Node.js,
# and outputs a comparison table.
#
# Usage: ./bench/bench_vs_node.sh

set -e

EDGEBOXC="./zig-out/bin/edgebox"
NODE="node"
RESULTS_FILE="bench/results_vs_node.md"

# Extract milliseconds from benchmark output
# Supports: "time: 1234ms", "12.3ms avg", "12.3ms total"
extract_ms() {
  local output="$1"
  local mode="$2"  # "total" or "single"

  if [ "$mode" = "single" ]; then
    # Single-run format: "time: 1234ms"
    echo "$output" | grep -oP '(?<=time: )\d+' | head -1
  else
    # Multi-run format: "123.4ms avg" — use avg
    local avg=$(echo "$output" | grep -oP '[\d.]+(?=ms avg)' | head -1)
    if [ -n "$avg" ]; then
      echo "$avg"
      return
    fi
    # Fallback: total
    echo "$output" | grep -oP '[\d.]+(?=ms total)' | head -1
  fi
}

# Benchmarks: file|description|time_mode
BENCHMARKS=(
  "bench/fib.js|fib(45) recursive|single"
  "bench/mandelbrot.js|mandelbrot 200x200|avg"
  "bench/prime_factors.js|prime factors 2K|avg"
  "bench/loop.js|array sum 1M|avg"
  "bench/gaussian_blur.js|gaussian blur|avg"
)

echo "# EdgeBox vs Node.js Benchmark Results" > "$RESULTS_FILE"
echo "" >> "$RESULTS_FILE"
echo "Date: $(date -u '+%Y-%m-%d %H:%M UTC')" >> "$RESULTS_FILE"
echo "Node: $($NODE --version)" >> "$RESULTS_FILE"
echo "Commit: $(git rev-parse --short HEAD)" >> "$RESULTS_FILE"
echo "" >> "$RESULTS_FILE"
echo "| Benchmark | EdgeBox | Node.js | Ratio | WASM funcs |" >> "$RESULTS_FILE"
echo "|-----------|---------|---------|-------|------------|" >> "$RESULTS_FILE"

for entry in "${BENCHMARKS[@]}"; do
  IFS='|' read -r file desc mode <<< "$entry"

  if [ ! -f "$file" ]; then
    echo "SKIP: $file not found"
    continue
  fi

  echo "--- $desc ---"

  # Binary path
  bin_name=$(basename "${file%.js}")
  bin_dir="zig-out/bin/$file"
  binary="$bin_dir/$bin_name"
  manifest="zig-out/cache/$file/standalone_manifest.json"

  # Compile if needed
  if [ ! -f "$binary" ]; then
    $EDGEBOXC --binary-only "$file" 2>&1 | grep -E 'Binary:|Standalone' || true
  fi

  wasm_count="0"
  if [ -f "$manifest" ]; then
    wasm_count=$(grep -o '"name"' "$manifest" 2>/dev/null | wc -l)
  fi

  if [ ! -f "$binary" ]; then
    echo "  SKIP: binary not found"
    echo "| $desc | SKIP | - | - | - |" >> "$RESULTS_FILE"
    continue
  fi

  # Run EdgeBox (3 runs, take best)
  eb_best=""
  for run in 1 2 3; do
    eb_out=$("$binary" 2>&1) || true
    eb_ms=$(extract_ms "$eb_out" "$mode")
    if [ -z "$eb_ms" ]; then continue; fi
    if echo "$eb_out" | grep -q "FAIL"; then
      eb_best="FAIL"
      break
    fi
    if [ -z "$eb_best" ] || [ "$(echo "$eb_ms < $eb_best" | bc 2>/dev/null)" = "1" ]; then
      eb_best="$eb_ms"
    fi
  done

  # Run Node.js (3 runs, take best)
  node_best=""
  for run in 1 2 3; do
    node_out=$($NODE "$file" 2>&1) || true
    node_ms=$(extract_ms "$node_out" "$mode")
    if [ -z "$node_ms" ]; then continue; fi
    if [ -z "$node_best" ] || [ "$(echo "$node_ms < $node_best" | bc 2>/dev/null)" = "1" ]; then
      node_best="$node_ms"
    fi
  done

  # Display results
  if [ -z "$eb_best" ] || [ "$eb_best" = "FAIL" ]; then
    echo "  EdgeBox: $eb_best"
    echo "| $desc | $eb_best | ${node_best:-?}ms | - | $wasm_count |" >> "$RESULTS_FILE"
    continue
  fi

  if [ -n "$eb_best" ] && [ -n "$node_best" ]; then
    ratio=$(echo "scale=2; $node_best / $eb_best" | bc 2>/dev/null || echo "?")
    # Determine if EdgeBox is faster or slower
    faster=$(echo "$eb_best < $node_best" | bc 2>/dev/null || echo "0")
    if [ "$faster" = "1" ]; then
      label="${ratio}x faster"
    else
      inv=$(echo "scale=2; $eb_best / $node_best" | bc 2>/dev/null || echo "?")
      label="${inv}x slower"
    fi
    echo "  EdgeBox: ${eb_best}ms | Node: ${node_best}ms | $label"
    echo "| $desc | ${eb_best}ms | ${node_best}ms | $label | $wasm_count |" >> "$RESULTS_FILE"
  else
    echo "  EdgeBox: ${eb_best:-?}ms | Node: ${node_best:-?}ms"
    echo "| $desc | ${eb_best:-?}ms | ${node_best:-?}ms | ? | $wasm_count |" >> "$RESULTS_FILE"
  fi
done

echo "" >> "$RESULTS_FILE"
echo "*Ratio: >1x = EdgeBox faster, <1x = Node faster*" >> "$RESULTS_FILE"

echo ""
echo "=== Results ==="
cat "$RESULTS_FILE"
