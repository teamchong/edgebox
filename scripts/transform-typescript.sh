#!/bin/bash
# Transform typescript.js to inject Zig structural check into isTypeRelatedTo.
# Applied at build time — no runtime string matching.
# Usage: ./scripts/transform-typescript.sh <input.js> <output.js>
set -e

INPUT="${1:-node_modules/typescript/lib/typescript.js}"
OUTPUT="${2:-src/workerd-tsc/typescript.js}"

if [ ! -f "$INPUT" ]; then
  echo "Error: $INPUT not found"
  exit 1
fi

echo "[transform] Injecting __edgebox_check_structural into isTypeRelatedTo..."

# Inject Zig structural pre-check after source === target
sed 's/if (source === target) {\n      return true;\n    }\n    if (relation !== identityRelation)/if (source === target) { return true; }\
    if (typeof __edgebox_check_structural==="function"\&\&source.id\&\&target.id){var __r=__edgebox_check_structural(source.id,target.id);if(__r===1)return true;if(__r===0)return false;}\
    if (relation !== identityRelation)/' "$INPUT" > "$OUTPUT" 2>/dev/null

# Verify injection worked
if grep -q "__edgebox_check_structural" "$OUTPUT"; then
  echo "[transform] Success — $(wc -c < "$OUTPUT") bytes"
else
  echo "[transform] Injection failed — copying original"
  cp "$INPUT" "$OUTPUT"
fi
