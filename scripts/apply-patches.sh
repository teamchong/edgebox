#!/usr/bin/env bash
# Apply EdgeBox patches to npm packages
# Usage: ./scripts/apply-patches.sh
#
# Per-package recipe system:
# - patches/typescript/ → patches for node_modules/typescript/
# - patches/webpack/    → patches for node_modules/webpack/ (future)
# - Each .patch file is applied with `patch -p1` from node_modules/
#
# Run after `npm install` to enable EdgeBox parallel for supported packages.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
PATCHES_DIR="$REPO_ROOT/patches"
NODE_MODULES="$REPO_ROOT/node_modules"

echo "=== Applying EdgeBox patches ==="

# Iterate each package directory in patches/
for pkg_dir in "$PATCHES_DIR"/*/; do
    pkg_name=$(basename "$pkg_dir")

        continue
    fi

    pkg_path="$NODE_MODULES/$pkg_name"
    if [ ! -d "$pkg_path" ]; then
        echo "  SKIP $pkg_name (not installed)"
        continue
    fi

    # Apply each .patch file
    for patch in "$pkg_dir"*.patch; do
        if [ -f "$patch" ]; then
            echo "  Applying: $pkg_name/$(basename "$patch")"
            cd "$NODE_MODULES"
            patch -p0 --forward --no-backup-if-mismatch < "$patch" 2>/dev/null || {
                echo "    (already applied or conflict)"
            }
            cd "$REPO_ROOT"
        fi
    done
done

echo "Done."
