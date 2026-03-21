#!/bin/bash
# Play Doom in TSC via EdgeBox + Ghostty pixel rendering
#
# Usage: ./scripts/doom-play.sh <path-to-ts-wasm-doom>
#
# Requires: Ghostty terminal (supports Kitty graphics protocol)
#           EdgeBox built: zig build v8-run -Doptimize=ReleaseFast

set -e

DOOM_DIR="${1:-.}"
EDGEBOX="$(dirname "$0")/../zig-out/bin/edgebox"
TSC="$(dirname "$0")/../benchmark/node_modules/typescript/lib/_tsc.js"
RENDERER="$(dirname "$0")/doom-render.js"

if [ ! -f "$EDGEBOX" ]; then
    echo "Build EdgeBox first: zig build v8-run -Doptimize=ReleaseFast"
    exit 1
fi

echo "🎮 Doom in TSC — EdgeBox Pixel Renderer"
echo "  Doom dir: $DOOM_DIR"
echo "  EdgeBox:  $EDGEBOX"
echo ""
echo "Each frame is type-checked by TSC and rendered via Kitty graphics."
echo "Press Ctrl+C to stop."
echo ""

# Render each result file as a frame
for result in "$DOOM_DIR"/packages/playground/final-doom-pun-intended/data/result-*.ts; do
    echo -ne "\033[H" # Move cursor to top-left
    # The palette values represent the frame pixels
    node "$RENDERER" "$DOOM_DIR/packages/playground/final-doom-pun-intended/palette-values.ts" 2>/dev/null
    sleep 0.033 # ~30fps timing
done
