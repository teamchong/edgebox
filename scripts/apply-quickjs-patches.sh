#!/usr/bin/env bash
# Apply QuickJS patches for frozen interpreter support
# This script is idempotent - can be run multiple times safely

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
QUICKJS_DIR="$REPO_ROOT/vendor/quickjs-ng"
PATCH_DIR="$REPO_ROOT/patches"

echo "=== Applying QuickJS patches ==="
echo "QuickJS directory: $QUICKJS_DIR"

cd "$QUICKJS_DIR"

# Reset to clean state
echo "[1/2] Resetting QuickJS to clean state..."
git reset --hard HEAD
git clean -fd

# Apply patches
echo "[2/2] Applying patches..."
for patch in "$PATCH_DIR"/*.patch; do
    echo "  Applying: $(basename "$patch")"
    if git apply "$patch"; then
        echo "  ✅ Success"
    else
        echo "  ❌ Failed to apply patch: $patch"
        echo "  Note: If patch is corrupt, run scripts/regenerate-quickjs-patch.sh to fix"
        exit 1
    fi
done

echo "✅ All patches applied successfully"
