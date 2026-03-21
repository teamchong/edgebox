#!/bin/bash
# Play Doom frames through EdgeBox — renders REAL WASM machine states
# as pixels in Ghostty via Kitty graphics protocol
#
# Usage: ./scripts/doom-play.sh [path-to-ts-wasm-doom] [scale]

DOOM_DIR="${1:-$HOME/Downloads/repos/typescript-types-only-wasm-runtime}"
SCALE="${2:-3}"
EDGEBOX="$(cd "$(dirname "$0")/.." && pwd)/zig-out/bin/edgebox"
RENDERER="$(cd "$(dirname "$0")" && pwd)/doom-extract-frame.js"
PALETTE_RENDERER="$(cd "$(dirname "$0")" && pwd)/doom-render.js"

clear
echo ""
echo "  DOOM in TypeScript Types — EdgeBox Renderer"
echo "  Scale: ${SCALE}x ($(( 320 * SCALE ))×$(( 200 * SCALE )))"
echo ""

# Render the title screen (pre-computed frame)
PALETTE="$DOOM_DIR/packages/playground/final-doom-pun-intended/palette-values.ts"
if [ -f "$PALETTE" ]; then
    "$EDGEBOX" "$PALETTE_RENDERER" "$PALETTE" "$SCALE"
    echo ""
    echo "  Title screen — computed by TypeScript type checker"
    echo "  Press Enter for WASM machine state frames..."
    read
fi

# Render each result file
RESULTS=$(ls "$DOOM_DIR"/packages/playground/final-doom-pun-intended/data/result-*.ts 2>/dev/null | sort)
FRAME=1
TOTAL=$(echo "$RESULTS" | wc -l | tr -d ' ')

for result in $RESULTS; do
    clear
    echo ""
    echo "  Frame $FRAME/$TOTAL: $(basename $result)"
    echo ""
    "$EDGEBOX" "$RENDERER" "$result" "$SCALE" 2>/dev/null
    echo ""
    echo "  Press Enter for next frame..."
    FRAME=$((FRAME + 1))
    read
done

echo ""
echo "  All frames rendered."
