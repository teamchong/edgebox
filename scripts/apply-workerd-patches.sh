#!/bin/bash
# Reset vendor/workerd to pinned commit and apply EdgeBox patches
set -e
cd "$(dirname "$0")/.."

WORKERD_DIR="vendor/workerd"
PATCH_DIR="patches/workerd"
PINNED_COMMIT="f30586680"  # v1.20260317.1

if [ ! -d "$WORKERD_DIR/.git" ]; then
  echo "[workerd] Initializing submodule..."
  git submodule update --init "$WORKERD_DIR"
fi

echo "[workerd] Resetting to pinned commit $PINNED_COMMIT..."
cd "$WORKERD_DIR"
git checkout "$PINNED_COMMIT" -- . 2>/dev/null || git checkout . 2>/dev/null
git clean -fd 2>/dev/null
cd ../..

# Apply patches
for patch in "$PATCH_DIR"/*.patch; do
  if [ -f "$patch" ]; then
    echo "[workerd] Applying $(basename $patch)..."
    cd "$WORKERD_DIR"
    patch -p1 < "../../$patch"
    cd ../..
  fi
done

echo "[workerd] Patches applied successfully"
