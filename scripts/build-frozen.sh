#!/bin/bash
# Build frozen_functions.o from frozen_functions.c
# Skips if already up-to-date (hash matches)
#
# Usage:
#   ./scripts/build-frozen.sh           # Build only
#   ./scripts/build-frozen.sh --commit  # Build and commit if changed

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
cd "$ROOT_DIR"

FROZEN_C="zig-out/cache/frozen_functions.c"
FROZEN_O="zig-out/cache/frozen_functions.o"
FROZEN_HASH="zig-out/cache/frozen_functions.sha256"

COMMIT=false
for arg in "$@"; do
    case $arg in
        --commit) COMMIT=true ;;
        *) echo "Unknown arg: $arg"; exit 1 ;;
    esac
done

# Check if frozen_functions.c exists
if [ ! -f "$FROZEN_C" ]; then
    echo "ERROR: $FROZEN_C not found"
    echo "Run 'zig build cli' first to generate it"
    exit 1
fi

# Calculate current hash
CURRENT_HASH=$(shasum -a 256 "$FROZEN_C" | awk '{print $1}')

# Check if .o exists and hash matches
if [ -f "$FROZEN_O" ] && [ -f "$FROZEN_HASH" ]; then
    STORED_HASH=$(cat "$FROZEN_HASH")
    if [ "$CURRENT_HASH" = "$STORED_HASH" ]; then
        echo "frozen_functions.o is up-to-date (hash: ${CURRENT_HASH:0:12}...)"
        exit 0
    fi
    echo "Hash mismatch - recompiling..."
else
    echo "frozen_functions.o missing or no hash - compiling..."
fi

# Compile frozen_functions.c to .o using zig's clang
# Match the exact flags used by build.zig for wasm32-wasi target
echo "Compiling frozen_functions.c (this takes ~18 minutes)..."
START_TIME=$(date +%s)

ZIG_PATH=$(which zig)
ZIG_LIB_DIR="$(dirname "$ZIG_PATH")/../lib"

"$ZIG_PATH" clang \
    -target wasm32-wasi \
    -O2 \
    -DNDEBUG \
    -D_GNU_SOURCE \
    -fno-sanitize=undefined \
    -D_WASI_EMULATED_SIGNAL \
    -DCONFIG_BIGNUM=0 \
    -I "$ROOT_DIR/vendor/quickjs-ng" \
    -c "$FROZEN_C" \
    -o "$FROZEN_O"

END_TIME=$(date +%s)
ELAPSED=$((END_TIME - START_TIME))
echo "Compiled in ${ELAPSED}s"

# Save hash
echo "$CURRENT_HASH" > "$FROZEN_HASH"
echo "Saved hash: ${CURRENT_HASH:0:12}..."

# Show result
ls -lh "$FROZEN_O"

# Commit if requested
if [ "$COMMIT" = true ]; then
    if git diff --quiet "$FROZEN_O" "$FROZEN_HASH" 2>/dev/null; then
        echo "No changes to commit"
    else
        echo "Committing frozen_functions.o..."
        git add "$FROZEN_O" "$FROZEN_HASH"
        git commit -m "chore: update pre-compiled frozen_functions.o

Hash: ${CURRENT_HASH:0:12}
Compile time: ${ELAPSED}s"
        echo "Committed!"
    fi
fi

echo "Done!"
