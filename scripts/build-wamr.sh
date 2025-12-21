#!/bin/bash
# Build WAMR (WebAssembly Micro Runtime) for the current platform
# This script is used by CI and can be run locally
set -e

# Detect platform and architecture
if [[ "$OSTYPE" == "darwin"* ]]; then
    PLATFORM="darwin"
    ARCH=$(uname -m)
else
    PLATFORM="linux"
    ARCH=$(uname -m)
fi

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
WAMR_DIR="${REPO_ROOT}/vendor/wamr"
BUILD_DIR="${WAMR_DIR}/product-mini/platforms/${PLATFORM}/build"
PATCHES_DIR="${REPO_ROOT}/patches/wamr"
PATCHES_MARKER="${WAMR_DIR}/.patches-applied"

# Determine configuration key (used to detect config changes)
# v10: Add GC support (required for AOT modules compiled with ref-types)
# v11: Add custom memory allocator hook for CoW instantiation
CONFIG_KEY="${PLATFORM}-${ARCH}-interpreter-v11"
CONFIG_MARKER="${BUILD_DIR}/.wamr_config"

# Apply patches to WAMR (if not already applied)
apply_patches() {
    if [ ! -f "$PATCHES_MARKER" ]; then
        echo "Applying WAMR patches..."
        cd "$WAMR_DIR"
        # Reset any local changes first
        git checkout . 2>/dev/null || true
        # Apply each patch
        for p in "$PATCHES_DIR"/*.patch; do
            if [ -f "$p" ]; then
                echo "  Applying $(basename "$p")..."
                patch -p1 --silent < "$p" || {
                    echo "Failed to apply patch: $p"
                    exit 1
                }
            fi
        done
        touch "$PATCHES_MARKER"
        echo "Patches applied successfully"
    fi
}

# Check if patches need to be reapplied (patch files changed)
check_patch_freshness() {
    if [ -f "$PATCHES_MARKER" ]; then
        for p in "$PATCHES_DIR"/*.patch; do
            if [ -f "$p" ] && [ "$p" -nt "$PATCHES_MARKER" ]; then
                echo "Patch file changed, re-applying patches..."
                rm -f "$PATCHES_MARKER"
                rm -rf "$BUILD_DIR"
                return
            fi
        done
    fi
}

# Ensure submodule is initialized
if [ ! -f "${WAMR_DIR}/CMakeLists.txt" ]; then
    echo "Initializing WAMR submodule..."
    cd "$REPO_ROOT"
    git submodule update --init vendor/wamr
fi

# Check patch freshness and apply patches
check_patch_freshness
apply_patches

# Skip if already built with matching config
if [ -f "${BUILD_DIR}/libiwasm.a" ] && [ -f "${CONFIG_MARKER}" ]; then
    EXISTING_CONFIG=$(cat "${CONFIG_MARKER}")
    if [ "$EXISTING_CONFIG" == "$CONFIG_KEY" ]; then
        echo "WAMR already built with config: ${CONFIG_KEY}"
        exit 0
    else
        echo "Config changed from ${EXISTING_CONFIG} to ${CONFIG_KEY}, rebuilding..."
        rm -rf "${BUILD_DIR}"
    fi
fi

echo "Building WAMR for ${PLATFORM} (${ARCH})..."
mkdir -p "${BUILD_DIR}"
cd "${BUILD_DIR}"

# Configuration: Fast interpreter with SIMD and GC support
# CRITICAL: SIMD MUST be enabled (WAMR_BUILD_SIMD=1)
# - WASM modules are compiled with simd128 feature (see build.zig)
# - AOT loading fails with "SIMD is not enabled" if WAMR lacks SIMD support
# - DO NOT disable SIMD for wizer or any other reason
# Instruction metering is needed for CPU instruction limiting in edgebox runtime.
# GC/ref-types is needed for AOT modules compiled with ref-types support.
echo "Configuring for ${PLATFORM} ${ARCH} with fast interpreter + SIMD + GC + CoW memory..."
cmake .. -DCMAKE_BUILD_TYPE=Release \
    -DWAMR_BUILD_JIT=0 \
    -DWAMR_BUILD_FAST_JIT=0 \
    -DWAMR_BUILD_SIMD=1 \
    -DWAMR_BUILD_FAST_INTERP=1 \
    -DWAMR_BUILD_GC=1 \
    -DWAMR_BUILD_INSTRUCTION_METERING=1 \
    -DWAMR_BUILD_ALLOC_WITH_USAGE=1

# Build with all available cores
make -j$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)

# Write config marker for cache validation
echo "${CONFIG_KEY}" > "${CONFIG_MARKER}"

echo "WAMR built successfully at ${BUILD_DIR}/libiwasm.a (config: ${CONFIG_KEY})"
