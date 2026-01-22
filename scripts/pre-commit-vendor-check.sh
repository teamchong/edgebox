#!/bin/bash
# Pre-commit hook to prevent direct modifications to vendor submodules
# Install: ln -sf ../../scripts/pre-commit-vendor-check.sh .git/hooks/pre-commit

set -e

RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check if any vendor submodules have staged changes
VENDOR_CHANGES=$(git diff --cached --name-only | grep -E '^vendor/(quickjs-ng|wamr|binaryen|metal0)/' || true)

if [ -n "$VENDOR_CHANGES" ]; then
    echo -e "${RED}ERROR: Direct modifications to vendor submodules detected!${NC}"
    echo ""
    echo "The following vendor files are staged for commit:"
    echo "$VENDOR_CHANGES" | sed 's/^/  /'
    echo ""
    echo -e "${YELLOW}IMPORTANT: Vendor submodules should NOT be modified directly.${NC}"
    echo ""
    echo "Instead, use patches:"
    echo "  1. Make your changes in the vendor directory"
    echo "  2. Generate a patch: cd vendor/<submodule> && git diff > ../../patches/<submodule>/NNN-description.patch"
    echo "  3. Reset the submodule: cd vendor/<submodule> && git checkout ."
    echo "  4. Update build.zig to apply the patch"
    echo "  5. Commit only the patch file"
    echo ""
    echo "Existing patches:"
    echo "  patches/*.patch           - QuickJS patches"
    echo "  patches/wamr/*.patch      - WAMR patches"
    echo ""
    echo "To bypass this check (NOT recommended): git commit --no-verify"
    exit 1
fi

# Check if submodule refs are being changed (this is OK, but warn)
SUBMODULE_CHANGES=$(git diff --cached --name-only | grep -E '^vendor/(quickjs-ng|wamr|binaryen|metal0)$' || true)

if [ -n "$SUBMODULE_CHANGES" ]; then
    echo -e "${YELLOW}WARNING: Submodule reference changes detected:${NC}"
    echo "$SUBMODULE_CHANGES" | sed 's/^/  /'
    echo ""
    echo "Make sure you're updating to an UPSTREAM commit, not a local commit with direct changes."
    echo "Use 'git submodule status' to verify the commit exists in the upstream repo."
    echo ""
fi

exit 0
