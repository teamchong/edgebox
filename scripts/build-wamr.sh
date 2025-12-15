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
BUILD_DIR="${REPO_ROOT}/vendor/wamr/product-mini/platforms/${PLATFORM}/build"

# Determine configuration key (used to detect config changes)
# v7: Use LLVM JIT for full SIMD support (Fast JIT + SIMD is unsupported combination)
CONFIG_KEY="${PLATFORM}-${ARCH}-simd-llvmjit-v7"
CONFIG_MARKER="${BUILD_DIR}/.wamr_config"

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

# Configuration: Enable LLVM JIT for full SIMD support during wizer pre-initialization
# Note: Fast JIT + SIMD is an unsupported combination in WAMR (see unsupported_combination.cmake)
# LLVM JIT provides complete SIMD opcode support through proper LLVM compilation.
# SIMDE is also enabled for interpreter fallback/compatibility.
# Instruction metering is needed for CPU instruction limiting in edgebox runtime.
if [[ "$PLATFORM" == "linux" ]]; then
    # Linux CI has LLVM installed via apt - use LLVM JIT for full SIMD support
    # Find system LLVM directory (llvm-18-dev is installed in CI)
    LLVM_DIR="/usr/lib/llvm-18/lib/cmake/llvm"
    if [ ! -d "$LLVM_DIR" ]; then
        # Fallback to finding LLVM dynamically
        LLVM_DIR=$(llvm-config-18 --cmakedir 2>/dev/null || echo "/usr/lib/llvm-18/lib/cmake/llvm")
    fi
    echo "Configuring for ${PLATFORM} ${ARCH} with LLVM JIT + SIMD (LLVM_DIR=${LLVM_DIR})..."
    cmake .. -DCMAKE_BUILD_TYPE=Release \
        -DLLVM_DIR="${LLVM_DIR}" \
        -DWAMR_BUILD_JIT=1 \
        -DWAMR_BUILD_FAST_JIT=0 \
        -DWAMR_BUILD_SIMD=1 \
        -DWAMR_BUILD_SIMDE=1 \
        -DWAMR_BUILD_INSTRUCTION_METERING=1
else
    # macOS: Use interpreter with SIMDE (no LLVM dependency)
    echo "Configuring for ${PLATFORM} ${ARCH} with SIMD + SIMDE (interpreter)..."
    cmake .. -DCMAKE_BUILD_TYPE=Release \
        -DWAMR_BUILD_JIT=0 \
        -DWAMR_BUILD_FAST_JIT=0 \
        -DWAMR_BUILD_SIMD=1 \
        -DWAMR_BUILD_SIMDE=1 \
        -DWAMR_BUILD_INSTRUCTION_METERING=1
fi

# Build with all available cores
make -j$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)

# Write config marker for cache validation
echo "${CONFIG_KEY}" > "${CONFIG_MARKER}"

echo "WAMR built successfully at ${BUILD_DIR}/libiwasm.a (config: ${CONFIG_KEY})"
