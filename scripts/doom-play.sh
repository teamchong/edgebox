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

# Save cursor position — all frames render at this spot
printf '\e7'

# Render the title screen first
PALETTE="$DOOM_DIR/packages/playground/final-doom-pun-intended/palette-values.ts"
if [ -f "$PALETTE" ]; then
    printf '\e8'  # restore cursor
    "$EDGEBOX" "$PALETTE_RENDERER" "$PALETTE" "$SCALE"
    echo "  DOOM in TypeScript Types — EdgeBox Renderer"
    sleep 3
fi

# Collect all frame files
RESULTS=$(ls "$DOOM_DIR"/packages/playground/final-doom-pun-intended/data/result-*.ts 2>/dev/null | sort)
TOTAL=$(echo "$RESULTS" | wc -l | tr -d ' ')

# Loop frames forever
while true; do
    FRAME=1
    for result in $RESULTS; do
        printf '\e8'  # restore cursor to saved position
        "$EDGEBOX" "$RENDERER" "$result" "$SCALE" 2>/dev/null
        echo "  Frame $FRAME/$TOTAL — $(basename $result)"
        FRAME=$((FRAME + 1))
        sleep 1
    done
done
