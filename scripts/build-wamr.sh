#!/bin/bash
set -e

PLATFORM="darwin"
ARCH=$(uname -m)
NPROC=$(sysctl -n hw.ncpu 2>/dev/null || echo 4)

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
WAMR_DIR="${REPO_ROOT}/vendor/wamr"
BUILD_DIR="${WAMR_DIR}/product-mini/platforms/${PLATFORM}/build"
PATCHES_DIR="${REPO_ROOT}/patches/wamr"
PREBUILT_DIR="${REPO_ROOT}/vendor/prebuilt/${PLATFORM}-${ARCH}/wamr"
PATCHES_MARKER="${WAMR_DIR}/.patches-applied"

echo "Building WAMR for ${PLATFORM} (${ARCH})..."

# Apply patches (only if not already applied)
echo "Checking WAMR patches..."
cd "$WAMR_DIR"
if [ ! -f "$PATCHES_MARKER" ]; then
    echo "Applying WAMR patches..."
    git checkout . 2>/dev/null || true
    for p in "$PATCHES_DIR"/*.patch; do
        if [ -f "$p" ]; then
            echo "  Applying $(basename "$p")..."
            patch -p1 --forward < "$p" || true
        fi
    done
    touch "$PATCHES_MARKER"
else
    echo "Patches already applied, skipping..."
fi

# Build
mkdir -p "$BUILD_DIR"
cd "$BUILD_DIR"

echo "Configuring for ${PLATFORM} ${ARCH} with fast interpreter + AOT + SIMD + GC + CoW memory..."
cmake .. \
    -DCMAKE_BUILD_TYPE=Release \
    -DWAMR_BUILD_INTERP=1 \
    -DWAMR_BUILD_FAST_INTERP=1 \
    -DWAMR_BUILD_AOT=1 \
    -DWAMR_BUILD_FAST_JIT=0 \
    -DWAMR_BUILD_LLVM_JIT=0 \
    -DWAMR_BUILD_SIMD=1 \
    -DWAMR_BUILD_BULK_MEMORY=1 \
    -DWASM_ENABLE_BULK_MEMORY_OPT=1 \
    -DWAMR_BUILD_GC=1 \
    -DWAMR_BUILD_REF_TYPES=1 \
    -DWAMR_ENABLE_INSTRUCTION_METERING=1 \
    -DWAMR_BUILD_LINEAR_MEMORY_CALLBACK=1 \
    2>&1

echo "Building..."
make -j${NPROC} 2>&1

# Copy to prebuilt
mkdir -p "$PREBUILT_DIR"
cp libiwasm.a "$PREBUILT_DIR/"

# Update hash
SOURCE_HASH=$(find "$WAMR_DIR/core" "$PATCHES_DIR" -type f -name "*.c" -o -name "*.h" -o -name "*.patch" | sort | xargs cat 2>/dev/null | shasum -a 256 | cut -d' ' -f1)
echo "$SOURCE_HASH" > "${REPO_ROOT}/vendor/prebuilt/${PLATFORM}-${ARCH}/.build-hash"

echo "Build complete! Library: $PREBUILT_DIR/libiwasm.a"
