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
# Save cursor at top
printf '\e7'

# Render the title screen first
PALETTE="$DOOM_DIR/packages/playground/final-doom-pun-intended/palette-values.ts"
if [ -f "$PALETTE" ]; then
    printf '\e8'
    "$EDGEBOX" "$PALETTE_RENDERER" "$PALETTE" "$SCALE"
    printf '\e[2K  DOOM in TypeScript Types — EdgeBox Renderer\n'
    printf '\e[2K  Title screen — type-checked by TypeScript\n'
    sleep 3
fi

# Collect all frame files
RESULTS=$(ls "$DOOM_DIR"/packages/playground/final-doom-pun-intended/data/result-*.ts 2>/dev/null | sort)
TOTAL=$(echo "$RESULTS" | wc -l | tr -d ' ')

# Loop frames forever (Ctrl+C to stop)
while true; do
    # Show title screen at the start of each cycle
    if [ -f "$PALETTE" ]; then
        printf '\e8'
        "$EDGEBOX" "$PALETTE_RENDERER" "$PALETTE" "$SCALE"
        printf '\e[2K  DOOM in TypeScript Types — EdgeBox Renderer\n'
        printf '\e[2K  Title screen — type-checked by TypeScript\n'
        sleep 3
    fi

    FRAME=1
    for result in $RESULTS; do
        printf '\e8'  # restore cursor to top
        "$EDGEBOX" "$RENDERER" "$result" "$SCALE" 2>/dev/null
        # Clear line before printing status (prevents text overlap)
        printf '\e[2K  Frame %d/%d — %s\n' "$FRAME" "$TOTAL" "$(basename "$result")"
        printf '\e[2K\n'
        FRAME=$((FRAME + 1))
        sleep 1
    done
done
