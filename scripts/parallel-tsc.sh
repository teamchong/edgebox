#!/bin/bash
# parallel-tsc.sh — Run TSC in parallel via workerd + Zig IO
#
# Usage: ./scripts/parallel-tsc.sh <project-dir> [tsc-args...]
# Example: ./scripts/parallel-tsc.sh ~/project --noEmit -p tsconfig.json
#
# Architecture:
#   1. Start workerd with parallel TSC config (main + 4 checker workers)
#   2. All workers share Zig IO (readFile/stat via mmap cache)
#   3. Main worker parses all files, dispatches shards to checkers
#   4. Checkers run type checking in parallel, return diagnostics
#   5. Main worker merges + deduplicates + outputs
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "${1:-.}" && pwd)"
shift 2>/dev/null || true
TSC_ARGS="${@:---noEmit -p tsconfig.json}"

EDGEBOX_DIR="$SCRIPT_DIR/.."
WORKERD_BIN="$EDGEBOX_DIR/vendor/workerd/bazel-bin/src/workerd/server/workerd"
CONFIG_DIR="$EDGEBOX_DIR/src/workerd-tsc"
ZIG_IO_LIB="$EDGEBOX_DIR/zig-out/lib/libedgebox_io.a"

# Check workerd binary exists
if [ ! -f "$WORKERD_BIN" ]; then
  echo "Error: workerd not built. Run: cd vendor/workerd && bazelisk build //src/workerd/server:workerd"
  exit 1
fi

# Build Zig IO if needed
if [ ! -f "$ZIG_IO_LIB" ]; then
  echo "[parallel-tsc] Building Zig IO library..."
  cd "$EDGEBOX_DIR"
  zig build-lib src/edgebox_workerd_io.zig \
    -target x86_64-linux-gnu \
    -OReleaseFast \
    --name edgebox_io \
    -femit-bin=zig-out/lib/libedgebox_io.a
fi

echo "[parallel-tsc] Project: $PROJECT_DIR"
echo "[parallel-tsc] Args: $TSC_ARGS"
echo "[parallel-tsc] Workers: 4 checkers"

# Start workerd with our config
# workerd reads the config.capnp which embeds our worker JS files
cd "$CONFIG_DIR"

# Set CWD for Zig IO to use the project directory
export EDGEBOX_CWD="$PROJECT_DIR"

START=$(date +%s%N)

# Run workerd serve — it starts all workers, main dispatches to checkers
"$WORKERD_BIN" serve config.capnp 2>&1 &
WORKERD_PID=$!

# Wait a moment for workerd to start
sleep 0.5

# Trigger the main worker via HTTP
curl -s -X POST http://127.0.0.1:8787/ \
  -H "Content-Type: application/json" \
  -d "{\"args\": [\"$TSC_ARGS\"], \"cwd\": \"$PROJECT_DIR\"}" | jq .

END=$(date +%s%N)
ELAPSED=$(( (END - START) / 1000000 ))
echo "[parallel-tsc] Wall time: ${ELAPSED}ms"

# Kill workerd
kill $WORKERD_PID 2>/dev/null
wait $WORKERD_PID 2>/dev/null || true
