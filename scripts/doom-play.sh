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

echo "DOOM in TypeScript Types — EdgeBox Renderer" >&2
echo "  Scale: ${SCALE}x ($(( 320 * SCALE ))×$(( 200 * SCALE )))" >&2
echo "" >&2

# Render the title screen (pre-computed frame)
PALETTE="$DOOM_DIR/packages/playground/final-doom-pun-intended/palette-values.ts"
if [ -f "$PALETTE" ]; then
    echo "=== Title Screen ===" >&2
    "$EDGEBOX" "$PALETTE_RENDERER" "$PALETTE" "$SCALE"
    echo "" >&2
    echo "Press Enter for WASM machine state frames..." >&2
    read
fi

# Render each result file (real WASM machine states computed by TSC)
RESULTS=$(ls "$DOOM_DIR"/packages/playground/final-doom-pun-intended/data/result-*.ts 2>/dev/null | sort)
FRAME=1
TOTAL=$(echo "$RESULTS" | wc -l)

for result in $RESULTS; do
    printf '\033[H' # cursor to top
    echo "Frame $FRAME/$TOTAL: $(basename $result) — extracting framebuffer..." >&2
    "$EDGEBOX" "$RENDERER" "$result" "$SCALE" 2>/dev/null
    FRAME=$((FRAME + 1))
    sleep 1
done

echo "" >&2
echo "All frames rendered." >&2
