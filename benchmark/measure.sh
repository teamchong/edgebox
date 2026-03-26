#!/usr/bin/env bash
# Honest benchmark: EdgeBox vs Node.js
# No --incremental, no caching tricks, same TS version, same project.
# Measures: correctness (diagnostic count) + performance (wall clock).
#
# Usage:
#   ./benchmark/measure.sh                    # default: playwright
#   ./benchmark/measure.sh rxjs               # specific project
#   RUNS=3 ./benchmark/measure.sh playwright  # multiple runs

set -uo pipefail
cd "$(dirname "$0")/.."
ROOT="$(pwd)"

PROJECT="${1:-playwright}"
RUNS="${RUNS:-1}"
FIXTURE="$ROOT/benchmark/fixtures/$PROJECT"

if [ ! -d "$FIXTURE" ]; then
  echo "ERROR: fixture not found: $FIXTURE"
  echo "Available: $(ls benchmark/fixtures/ 2>/dev/null | tr '\n' ' ')"
  exit 1
fi

# Find tsconfig
TSCONFIG=""
for f in "$FIXTURE/tsconfig.json" "$FIXTURE/packages/playwright-core/tsconfig.json"; do
  [ -f "$f" ] && TSCONFIG="$f" && break
done
if [ -z "$TSCONFIG" ]; then
  TSCONFIG=$(find "$FIXTURE" -maxdepth 2 -name tsconfig.json | head -1)
fi
if [ -z "$TSCONFIG" ]; then
  echo "ERROR: no tsconfig.json found in $FIXTURE"
  exit 1
fi
TSCONFIG_DIR="$(dirname "$TSCONFIG")"

TS_VERSION=$(node -e "console.log(require('./node_modules/typescript/package.json').version)")
EDGEBOX_BIN="$ROOT/zig-out/bin/edgebox"

echo "=== EdgeBox Honest Benchmark ==="
echo "Project:    $PROJECT"
echo "Fixture:    $FIXTURE"
echo "tsconfig:   $TSCONFIG"
echo "TypeScript: $TS_VERSION"
echo "Runs:       $RUNS"
echo ""

# ─────────────────────────────────────────────
# 1. Node.js baseline (gold standard)
# ─────────────────────────────────────────────
echo "--- Node.js TSC (baseline) ---"
NODE_DIAGS_FILE=$(mktemp)
for i in $(seq 1 "$RUNS"); do
  START=$(date +%s%N)
  node "$ROOT/node_modules/typescript/lib/tsc.js" -p "$TSCONFIG" --noEmit 2>&1 | tee "$NODE_DIAGS_FILE" | tail -1
  END=$(date +%s%N)
  ELAPSED=$(( (END - START) / 1000000 ))
  NODE_COUNT=$(grep -c "error TS" "$NODE_DIAGS_FILE" 2>/dev/null || echo 0)
  echo "  Run $i: ${ELAPSED}ms, $NODE_COUNT diagnostics"
done
NODE_DIAG_COUNT=$(grep -c "error TS" "$NODE_DIAGS_FILE" 2>/dev/null || echo 0)
echo "  Node.js diagnostics: $NODE_DIAG_COUNT"
echo ""

# ─────────────────────────────────────────────
# 2. EdgeBox (daemon mode)
# ─────────────────────────────────────────────
if [ ! -x "$EDGEBOX_BIN" ]; then
  echo "SKIP: EdgeBox binary not found at $EDGEBOX_BIN"
  echo "Build with: zig build"
  exit 1
fi

echo "--- EdgeBox (daemon) ---"

# Kill any existing daemon
kill -9 "$(cat /tmp/edgebox-daemon.pid 2>/dev/null)" 2>/dev/null || true
rm -f /tmp/edgebox.sock /tmp/edgebox-daemon.pid
# Remove incremental cache so --incremental doesn't help
rm -rf /tmp/edgebox-incr-cache
sleep 1

EB_DIAGS_FILE=$(mktemp)
for i in $(seq 1 "$RUNS"); do
  # Remove incremental cache EVERY run — no cheating
  rm -rf /tmp/edgebox-incr-cache
  START=$(date +%s%N)
  (cd "$TSCONFIG_DIR" && "$EDGEBOX_BIN" tsc 2>&1) | tee "$EB_DIAGS_FILE" | tail -1
  END=$(date +%s%N)
  ELAPSED=$(( (END - START) / 1000000 ))
  EB_COUNT=$(grep -c "error TS" "$EB_DIAGS_FILE" 2>/dev/null || echo 0)
  if [ "$i" -eq 1 ]; then
    echo "  Run $i: ${ELAPSED}ms, $EB_COUNT diagnostics (daemon cold — includes daemon startup)"
  else
    echo "  Run $i: ${ELAPSED}ms, $EB_COUNT diagnostics (daemon warm)"
  fi
done
EB_DIAG_COUNT=$(grep -c "error TS" "$EB_DIAGS_FILE" 2>/dev/null || echo 0)
echo "  EdgeBox diagnostics: $EB_DIAG_COUNT"
echo ""

# ─────────────────────────────────────────────
# 3. Correctness check
# ─────────────────────────────────────────────
echo "--- Correctness ---"
# Normalize paths for comparison
grep "error TS" "$NODE_DIAGS_FILE" | sed 's/   .*//' | sort > /tmp/bench_node_diags.txt
grep "error TS" "$EB_DIAGS_FILE" | sed "s|$FIXTURE/||g; s|$ROOT/benchmark/fixtures/$PROJECT/||g" | sed 's/   .*//' | sort > /tmp/bench_eb_diags.txt

NODE_N=$(wc -l < /tmp/bench_node_diags.txt)
EB_N=$(wc -l < /tmp/bench_eb_diags.txt)

if [ "$NODE_N" -eq "$EB_N" ]; then
  DIFF_COUNT=$(diff /tmp/bench_node_diags.txt /tmp/bench_eb_diags.txt | grep -c "^[<>]" 2>/dev/null || true)
  DIFF_COUNT="${DIFF_COUNT:-0}"
  if [ "$DIFF_COUNT" = "0" ]; then
    echo "  PASS: $NODE_N/$EB_N diagnostics match exactly"
  else
    echo "  PARTIAL: same count ($NODE_N) but $DIFF_COUNT lines differ"
    echo "  Differences:"
    diff /tmp/bench_node_diags.txt /tmp/bench_eb_diags.txt | head -10
  fi
else
  echo "  FAIL: Node=$NODE_N EdgeBox=$EB_N"
  echo "  Extra in EdgeBox (not in Node):"
  comm -23 /tmp/bench_eb_diags.txt /tmp/bench_node_diags.txt | head -5
  echo "  Missing in EdgeBox (in Node but not EB):"
  comm -13 /tmp/bench_eb_diags.txt /tmp/bench_node_diags.txt | head -5
fi

# Cleanup
rm -f "$NODE_DIAGS_FILE" "$EB_DIAGS_FILE"

echo ""
echo "--- Summary ---"
echo "  Node.js:  $NODE_N diagnostics"
echo "  EdgeBox:  $EB_N diagnostics"
echo "  Match:    $([ "$NODE_N" -eq "$EB_N" ] && echo 'YES' || echo 'NO')"
