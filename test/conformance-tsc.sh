#!/bin/bash
# Conformance test: EdgeBox V8 pool TSC vs Node.js TSC
# Uses V8 pool daemon with Unix socket IPC.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
EDGEBOX="$ROOT/zig-out/bin/edgebox"
PASS=0; FAIL=0; SKIP=0

[ -f "$EDGEBOX" ] || { echo "ERROR: edgebox binary not found. Build with: zig build edgebox-cli"; exit 1; }

# Start daemon
"$EDGEBOX" daemon run &
DAEMON_PID=$!
sleep 15

PROJECTS=(
  "rxjs:packages/rxjs/tsconfig.json"
  "trpc:tsconfig.json"
  "date-fns:tsconfig.json"
  "typeorm:tsconfig.json"
  "playwright:tsconfig.json"
  "vscode:src/tsconfig.json"
)

for entry in "${PROJECTS[@]}"; do
  project="${entry%%:*}"
  tsconfig="${entry#*:}"
  PROJECT_DIR="$ROOT/benchmark/fixtures/$project"
  TSCONFIG_DIR="$(dirname "$PROJECT_DIR/$tsconfig")"

  [ -f "$PROJECT_DIR/$tsconfig" ] || { echo "SKIP $project"; SKIP=$((SKIP+1)); continue; }

  # EdgeBox (via daemon Unix socket)
  EB_COUNT=$(cd "$TSCONFIG_DIR" && "$EDGEBOX" tsc 2>/dev/null | grep -c "error TS" 2>/dev/null || echo 0)

  # Node.js
  NODE_COUNT=$(cd "$TSCONFIG_DIR" && npx tsc --noEmit -p tsconfig.json 2>&1 | grep -c "error TS" 2>/dev/null || echo 0)

  if [ "$EB_COUNT" = "$NODE_COUNT" ]; then
    echo "PASS $project: $EB_COUNT = $NODE_COUNT"
    PASS=$((PASS+1))
  else
    echo "FAIL $project: EdgeBox=$EB_COUNT Node=$NODE_COUNT"
    FAIL=$((FAIL+1))
  fi
done

kill $DAEMON_PID 2>/dev/null; wait $DAEMON_PID 2>/dev/null || true
rm -f /tmp/edgebox.sock

echo ""
echo "Results: $PASS passed, $FAIL failed, $SKIP skipped"
[ "$FAIL" -eq 0 ]
