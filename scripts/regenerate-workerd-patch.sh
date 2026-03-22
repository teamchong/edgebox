#!/bin/bash
# Regenerate workerd patch from current vendor/workerd changes
# Does NOT reset — run after testing your changes
set -e
cd "$(dirname "$0")/.."

WORKERD_DIR="vendor/workerd"
PATCH_DIR="patches/workerd"

mkdir -p "$PATCH_DIR"

cd "$WORKERD_DIR"
git add -A
git diff --cached > "../../$PATCH_DIR/001-edgebox-io-bridge.patch"
git reset HEAD . >/dev/null 2>&1

echo "[workerd] Patch regenerated: $PATCH_DIR/001-edgebox-io-bridge.patch"
echo "  $(wc -l < ../../$PATCH_DIR/001-edgebox-io-bridge.patch) lines"
