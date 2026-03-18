#!/bin/bash
# edgebox pack --workerd — Create a standalone workerd binary
#
# Usage:
#   ./scripts/pack-workerd.sh <worker-dir>
#   ./scripts/pack-workerd.sh zig-out/bin/app.js
#
# Produces: <worker-dir>/<name>-workerd (single executable)
#
# Requirements: workerd (npm install workerd, or brew install workerd)

set -euo pipefail

WORKER_DIR="${1:?Usage: pack-workerd.sh <worker-dir>}"

# Find the config.capnp and worker .mjs
CONFIG=$(find "$WORKER_DIR" -name '*-config.capnp' -maxdepth 1 | head -1)
WORKER_MJS=$(find "$WORKER_DIR" -name '*-worker.mjs' -maxdepth 1 | head -1)
STANDALONE_WASM=$(find "$WORKER_DIR" -name '*-standalone.wasm' -maxdepth 1 | head -1)

if [ -z "$CONFIG" ]; then
  echo "Error: No *-config.capnp found in $WORKER_DIR"
  echo "Run 'edgebox <app.js>' first to generate worker files."
  exit 1
fi

if [ -z "$WORKER_MJS" ]; then
  echo "Error: No *-worker.mjs found in $WORKER_DIR"
  exit 1
fi

BASENAME=$(basename "$WORKER_MJS" -worker.mjs)
OUTPUT="$WORKER_DIR/${BASENAME}-workerd"
WORKERD_MJS="$WORKER_DIR/${BASENAME}-workerd-worker.mjs"
WORKERD_CONFIG="$WORKER_DIR/${BASENAME}-workerd-config.capnp"

# Find workerd binary (direct install or npx)
WORKERD=""
if command -v workerd &>/dev/null; then
  WORKERD="workerd"
elif npx workerd --version &>/dev/null 2>&1; then
  WORKERD="npx workerd"
else
  echo "Error: workerd not found. Install with:"
  echo "  npm install workerd"
  exit 1
fi

echo "=== EdgeBox Pack (workerd) ==="
echo "  Worker:  $WORKER_MJS ($(wc -c < "$WORKER_MJS" | tr -d ' ') bytes)"
echo "  WASM:    ${STANDALONE_WASM:-none} (${STANDALONE_WASM:+$(wc -c < "$STANDALONE_WASM" | tr -d ' ') bytes})"
echo "  Output:  $OUTPUT"
echo ""

# Step 1: Create workerd-compatible worker (strip Node.js imports, keep else branch)
echo "Step 1: Creating workerd-compatible worker..."
python3 -c "
import sys, re

content = open('$WORKER_MJS').read()

# Remove Node.js static imports
content = re.sub(r\"^import \{[^}]+\} from '[^']+';\\n\", '', content, flags=re.MULTILINE)
content = re.sub(r\"^const require = createRequire\([^)]+\);\\n\", '', content, flags=re.MULTILINE)

# Replace the if/else Node.js detection block:
# Keep only the else branch (workerd path) for WASM loading
content = re.sub(
    r'if \(typeof process !== .undefined. && process\.versions\?\.node\) \{[^}]+\} else \{',
    '{',
    content,
    count=1
)

sys.stdout.write(content)
" > "$WORKERD_MJS"

echo "  Created: $WORKERD_MJS ($(wc -c < "$WORKERD_MJS" | tr -d ' ') bytes)"

# Step 2: Create workerd config pointing to the workerd worker
echo "Step 2: Creating workerd config..."
WASM_FILENAME=$(basename "$STANDALONE_WASM" 2>/dev/null || echo "")
WORKERD_MJS_BASE=$(basename "$WORKERD_MJS")
cat > "$WORKERD_CONFIG" << EOF
using Workerd = import "/workerd/workerd.capnp";

const config :Workerd.Config = (
  services = [
    (name = "main", worker = .worker),
  ],
  sockets = [
    (name = "http", address = "*:8787", http = (), service = "main"),
  ],
);

const worker :Workerd.Worker = (
  modules = [
    (name = "entrypoint", esModule = embed "$WORKERD_MJS_BASE"),
$([ -n "$WASM_FILENAME" ] && echo "    (name = \"$WASM_FILENAME\", wasm = embed \"$WASM_FILENAME\"),")
  ],
  compatibilityDate = "2024-09-23",
  compatibilityFlags = ["nodejs_compat"],
);
EOF

# Step 3: Compile with workerd
echo "Step 3: Compiling with workerd..."
$WORKERD compile "$WORKERD_CONFIG" > "$OUTPUT"
chmod +x "$OUTPUT"

# Cleanup temp files
rm -f "$WORKERD_MJS" "$WORKERD_CONFIG"

echo ""
echo "=== Done ==="
echo "  Binary: $OUTPUT ($(du -h "$OUTPUT" | cut -f1))"
echo "  Run:    $OUTPUT"
