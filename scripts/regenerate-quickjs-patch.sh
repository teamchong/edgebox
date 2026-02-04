#!/usr/bin/env bash
# Regenerate QuickJS patch from current vendor/quickjs-ng changes
# Usage: ./scripts/regenerate-quickjs-patch.sh
#
# Prerequisites:
# 1. Run ./scripts/apply-quickjs-patches.sh first
# 2. Make your edits to vendor/quickjs-ng files
# 3. Run this script to save changes to patch file
#
# This script does NOT reset vendor - you must test your changes first!

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
QUICKJS_DIR="$REPO_ROOT/vendor/quickjs-ng"
PATCH_FILE="$REPO_ROOT/patches/quickjs/001-frozen-interpreter-all.patch"

# Pinned commit - must match git submodule
QUICKJS_COMMIT="fa9472db3607d9682755ab0e73690297fff8a811"

echo "=== Regenerating QuickJS patch ==="

cd "$QUICKJS_DIR"

# Generate patch against pinned commit
echo "Generating patch against $QUICKJS_COMMIT..."
git diff "$QUICKJS_COMMIT" > "$PATCH_FILE"

echo ""
echo "Patch saved to: patches/quickjs/001-frozen-interpreter-all.patch"
echo "Lines: $(wc -l < "$PATCH_FILE")"
echo ""
echo "=== NEXT STEPS ==="
echo "1. Build and test:  zig build cli"
echo "2. Test your changes thoroughly"
echo "3. If working:      git add patches/quickjs/001-frozen-interpreter-all.patch"
echo "4. Commit:          git commit -m 'fix(quickjs): your change description'"
echo "5. Reset vendor:    ./scripts/apply-quickjs-patches.sh"
