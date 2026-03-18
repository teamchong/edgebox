#!/bin/bash
# edgebox pack — Create a Node.js SEA (Single Executable Application)
#
# Usage:
#   ./scripts/pack.sh <worker-dir>
#   ./scripts/pack.sh zig-out/bin/benchmark/node_modules/typescript/lib/_tsc.js
#
# Produces: <worker-dir>/<name>-sea (single executable)
#
# Requirements: Node.js 20+ with SEA support, postject

set -euo pipefail

WORKER_DIR="${1:?Usage: pack.sh <worker-dir>}"

# Find the worker .mjs and standalone .wasm
WORKER_MJS=$(find "$WORKER_DIR" -name '*-worker.mjs' -maxdepth 1 | head -1)
STANDALONE_WASM=$(find "$WORKER_DIR" -name '*-standalone.wasm' -maxdepth 1 | head -1)

if [ -z "$WORKER_MJS" ]; then
  echo "Error: No *-worker.mjs found in $WORKER_DIR"
  exit 1
fi

BASENAME=$(basename "$WORKER_MJS" -worker.mjs)
SEA_ENTRY="$WORKER_DIR/${BASENAME}-sea-entry.js"
SEA_CONFIG="$WORKER_DIR/${BASENAME}-sea-config.json"
SEA_BLOB="$WORKER_DIR/${BASENAME}-sea-prep.blob"
SEA_BINARY="$WORKER_DIR/${BASENAME}-sea"

echo "=== EdgeBox Pack ==="
echo "  Worker:  $WORKER_MJS ($(wc -c < "$WORKER_MJS" | tr -d ' ') bytes)"
echo "  WASM:    ${STANDALONE_WASM:-none} (${STANDALONE_WASM:+$(wc -c < "$STANDALONE_WASM" | tr -d ' ') bytes})"
echo "  Output:  $SEA_BINARY"
echo ""

# Step 1: Create SEA entry that inlines WASM and patches the worker
echo "Step 1: Creating SEA entry..."

# Read the worker .mjs content
WORKER_CONTENT=$(cat "$WORKER_MJS")

# If standalone WASM exists, inline it as base64
if [ -n "$STANDALONE_WASM" ] && [ -f "$STANDALONE_WASM" ]; then
  WASM_B64=$(base64 -w0 "$STANDALONE_WASM" 2>/dev/null || base64 "$STANDALONE_WASM")
  WASM_FILENAME=$(basename "$STANDALONE_WASM")

  # Create entry that patches the WASM loading to use inline base64
  cat > "$SEA_ENTRY" << 'ENTRY_HEADER'
// EdgeBox SEA — Single Executable Application (auto-generated)
// Patches WASM loading to use inline base64 instead of file read
const { readFileSync: _origReadFileSync } = require('fs');
const { join: _origJoin } = require('path');

ENTRY_HEADER

  # Add inline WASM
  echo "const __INLINE_WASM = Buffer.from('${WASM_B64}', 'base64');" >> "$SEA_ENTRY"
  echo "const __WASM_FILENAME = '${WASM_FILENAME}';" >> "$SEA_ENTRY"
  echo "" >> "$SEA_ENTRY"

  cat >> "$SEA_ENTRY" << 'ENTRY_PATCH'
// Provide readFileSync that intercepts WASM loading (returns inline base64)
const _fs = require('fs');
const _origRead = _fs.readFileSync.bind(_fs);
const readFileSync = function(path, ...args) {
  if (typeof path === 'string' && path.endsWith(__WASM_FILENAME)) {
    return __INLINE_WASM;
  }
  return _origRead(path, ...args);
};

// CJS equivalents for ESM APIs
const _url = require('url');
const _path = require('path');
const _module = require('module');
ENTRY_PATCH

  # Convert the ESM worker to CJS-compatible form
  # 1. Replace ESM imports with CJS requires
  # 2. Strip the workerd branch (await import is invalid in CJS)
  # 3. Remove the if/else condition — always use Node.js path
  python3 -c "
import sys, re

content = open('$WORKER_MJS').read()

# Replace ESM imports with CJS
content = content.replace(\"import { readFileSync } from 'fs';\", '// (patched above)')
content = content.replace(\"import { fileURLToPath } from 'url';\", '')
content = content.replace(\"import { dirname, join } from 'path';\", \"const { dirname, join } = require('path');\")
content = content.replace(\"import { createRequire } from 'module';\", '')
content = re.sub(r'const require = createRequire\(.*?\);', '', content)

# Replace import.meta.url with CJS equivalent
content = content.replace('import.meta.url', 'require(\"url\").pathToFileURL(__filename).href')

# Replace __dir computation
content = re.sub(
    r'const __dir = dirname\(fileURLToPath\(.*?\)\);',
    'const __dir = __dirname;',
    content
)

# Remove the workerd else branch (contains await import which is invalid in CJS)
# '} else {' has the closing } for the if block — keep it, strip ' else {'
lines = content.split('\n')
out_lines = []
skip_until_closing = False
for i, line in enumerate(lines):
    if skip_until_closing:
        if line.strip() == '}':
            skip_until_closing = False
        continue
    if '} else {' in line:
        rest = '\n'.join(lines[i:i+5])
        if 'await import' in rest:
            # Keep the closing } but remove ' else {'
            out_lines.append(line.replace(' else {', ''))
            skip_until_closing = True
            continue
    out_lines.append(line)
content = '\n'.join(out_lines)

# Remove export statements
content = re.sub(r'^export ', '', content, flags=re.MULTILINE)
content = re.sub(r'^export\{', '// export{', content, flags=re.MULTILINE)

sys.stdout.write(content)
" >> "$SEA_ENTRY"

else
  # No WASM — just convert ESM to CJS
  echo "$WORKER_CONTENT" | \
    sed "s|import { readFileSync } from 'fs';|const { readFileSync } = require('fs');|" | \
    sed "s|import { fileURLToPath } from 'url';|const { fileURLToPath } = require('url');|" | \
    sed "s|import { dirname, join } from 'path';|const { dirname, join } = require('path');|" | \
    sed "s|import { createRequire } from 'module';||" | \
    sed "s|const require = createRequire(import.meta.url);||" | \
    sed "s|import.meta.url|require('url').pathToFileURL(__filename).href|g" | \
    sed 's/^export //' > "$SEA_ENTRY"
fi

echo "  Created: $SEA_ENTRY ($(wc -c < "$SEA_ENTRY" | tr -d ' ') bytes)"

# Step 2: Create SEA config
echo "Step 2: Creating SEA config..."
cat > "$SEA_CONFIG" << EOF
{
  "main": "$SEA_ENTRY",
  "output": "$SEA_BLOB",
  "disableExperimentalSEAWarning": true,
  "useSnapshot": false,
  "useCodeCache": true
}
EOF

# Step 3: Generate blob
echo "Step 3: Generating SEA blob..."
node --experimental-sea-config "$SEA_CONFIG"
echo "  Blob: $SEA_BLOB ($(wc -c < "$SEA_BLOB" | tr -d ' ') bytes)"

# Step 4: Copy Node.js binary
echo "Step 4: Copying Node.js binary..."
NODE_BIN=$(which node)
cp "$NODE_BIN" "$SEA_BINARY"

# Step 5: Remove codesign (macOS only)
if [[ "$OSTYPE" == "darwin"* ]]; then
  echo "Step 5: Removing code signature (macOS)..."
  codesign --remove-signature "$SEA_BINARY" 2>/dev/null || true
fi

# Step 6: Inject blob
echo "Step 6: Injecting SEA blob..."
npx postject "$SEA_BINARY" NODE_SEA_BLOB "$SEA_BLOB" \
  --sentinel-fuse NODE_SEA_FUSE_fce680ab2cc467b6e072b8b5df1996b2 \
  --overwrite

# Step 7: Re-sign (macOS only)
if [[ "$OSTYPE" == "darwin"* ]]; then
  echo "Step 7: Re-signing (macOS)..."
  codesign --sign - "$SEA_BINARY" 2>/dev/null || true
fi

# Cleanup temp files
rm -f "$SEA_ENTRY" "$SEA_CONFIG" "$SEA_BLOB"

echo ""
echo "=== Done ==="
echo "  Binary: $SEA_BINARY ($(wc -c < "$SEA_BINARY" | tr -d ' ') bytes)"
echo "  Run:    $SEA_BINARY --version"
