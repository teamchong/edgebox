#!/usr/bin/env bash
# Apply QuickJS patches for frozen interpreter support
# Usage: ./scripts/apply-quickjs-patches.sh
#
# This script:
# 1. Resets vendor/quickjs-ng to the pinned commit (discards ALL local changes)
# 2. Applies patches from patches/quickjs/

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
QUICKJS_DIR="$REPO_ROOT/vendor/quickjs-ng"
PATCH_DIR="$REPO_ROOT/patches/quickjs"

# Pinned commit - must match git submodule
QUICKJS_COMMIT="fa9472db3607d9682755ab0e73690297fff8a811"

echo "=== Applying QuickJS patches ==="

cd "$QUICKJS_DIR"

# Reset to pinned commit (discards ALL local changes including staged/committed)
echo "[1/2] Resetting to pinned commit $QUICKJS_COMMIT..."
git reset --hard "$QUICKJS_COMMIT"
git clean -fd

# Apply patches
echo "[2/2] Applying patches..."
for patch in "$PATCH_DIR"/*.patch; do
    if [ -f "$patch" ]; then
        echo "  Applying: $(basename "$patch")"
        patch -p1 < "$patch"
    fi
done

echo ""
echo "Done. You can now edit vendor/quickjs-ng files directly."
echo "After editing, run: ./scripts/regenerate-quickjs-patch.sh"
