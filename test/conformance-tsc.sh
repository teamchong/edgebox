#!/bin/bash
# Conformance test: EdgeBox workerd TSC vs Node.js TSC
# Verifies diagnostic counts match exactly on all benchmark projects.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
WORKERD="$ROOT/vendor/workerd/bazel-bin/src/workerd/server/workerd"
CAPNP="$ROOT/workerd-test.capnp"
PASS=0; FAIL=0; SKIP=0

[ -f "$WORKERD" ] || { echo "ERROR: workerd not found. Build first."; exit 1; }

# Create capnp config
cat > "$CAPNP" << 'EOF'
using Workerd = import "/workerd/workerd.capnp";
const config :Workerd.Config = (
  services = [(name = "main", worker = .mainWorker)],
  sockets = [(name = "main", address = "*:18787", service = "main")],
);
const mainWorker :Workerd.Worker = (
  modules = [
    (name = "worker", esModule = embed "src/workerd-tsc/checker-parallel.js"),
    (name = "bootstrap.js", esModule = embed "src/workerd-tsc/bootstrap.js"),
    (name = "typescript.js", esModule = embed "node_modules/typescript/lib/typescript.js"),
  ],
  compatibilityDate = "2024-01-01",
);
EOF

run_edgebox() {
  local cwd="$1"
  echo "{\"cwd\":\"$cwd\"}" > /tmp/edgebox-project-config.json
  local out="/tmp/edgebox-conformance-out.txt"
  cd "$ROOT"
  "$WORKERD" serve "$CAPNP" > "$out" 2>&1 &
  local pid=$!
  sleep 3
  curl -s http://localhost:18787/ > /dev/null 2>&1 || true
  # TSC calls process.exit → workerd exits. Wait up to 120s.
  local waited=0
  while kill -0 $pid 2>/dev/null && [ $waited -lt 120 ]; do
    sleep 1; waited=$((waited+1))
  done
  kill $pid 2>/dev/null; wait $pid 2>/dev/null || true
  grep -c "error TS" "$out" 2>/dev/null || echo 0
}

run_node() {
  local dir="$1"
  cd "$dir"
  local out
  out=$(npx tsc --noEmit -p tsconfig.json 2>&1 || true)
  echo "$out" | grep -c "error TS" 2>/dev/null || echo 0
}

# Project definitions (name:tsconfig_path relative to fixture dir)
declare -A PROJECTS=(
  [rxjs]="packages/rxjs/tsconfig.json"
  [trpc]="tsconfig.json"
  [date-fns]="tsconfig.json"
  [typeorm]="tsconfig.json"
  [playwright]="tsconfig.json"
  [vscode]="src/tsconfig.json"
)
ORDER=(rxjs trpc date-fns typeorm playwright vscode)

for project in "${ORDER[@]}"; do
  tsconfig="${PROJECTS[$project]}"
  PROJECT_DIR="$ROOT/benchmark/fixtures/$project"
  TSCONFIG_DIR="$(dirname "$PROJECT_DIR/$tsconfig")"

  [ -f "$PROJECT_DIR/$tsconfig" ] || { echo "SKIP $project"; SKIP=$((SKIP+1)); continue; }

  EB_COUNT=$(run_edgebox "$TSCONFIG_DIR")
  NODE_COUNT=$(run_node "$TSCONFIG_DIR")

  if [ "$EB_COUNT" = "$NODE_COUNT" ]; then
    echo "PASS $project: $EB_COUNT = $NODE_COUNT"
    PASS=$((PASS+1))
  else
    echo "FAIL $project: EdgeBox=$EB_COUNT Node=$NODE_COUNT"
    FAIL=$((FAIL+1))
  fi
done

echo ""
echo "Results: $PASS passed, $FAIL failed, $SKIP skipped"
[ "$FAIL" -eq 0 ]
