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

# Skip if already built
if [ -f "${BUILD_DIR}/libiwasm.a" ]; then
    echo "WAMR already built at ${BUILD_DIR}/libiwasm.a"
    exit 0
fi

echo "Building WAMR for ${PLATFORM} (${ARCH})..."
mkdir -p "${BUILD_DIR}"
cd "${BUILD_DIR}"

# Platform-specific configuration:
# - Linux x86_64: Native SIMD support (SSE/AVX), no SIMDE needed
# - Darwin ARM64: Needs SIMDE for interpreter SIMD support
# - Both: SIMD enabled, Fast JIT disabled, Instruction Metering enabled
if [[ "$PLATFORM" == "linux" && "$ARCH" == "x86_64" ]]; then
    echo "Configuring for Linux x86_64 with native SIMD..."
    cmake .. -DCMAKE_BUILD_TYPE=Release \
        -DWAMR_BUILD_FAST_JIT=0 \
        -DWAMR_BUILD_SIMD=1 \
        -DWAMR_BUILD_SIMDE=0 \
        -DWAMR_BUILD_INSTRUCTION_METERING=1
else
    echo "Configuring for ${PLATFORM} ${ARCH} with SIMDE..."
    cmake .. -DCMAKE_BUILD_TYPE=Release \
        -DWAMR_BUILD_FAST_JIT=0 \
        -DWAMR_BUILD_SIMD=1 \
        -DWAMR_BUILD_SIMDE=1 \
        -DWAMR_BUILD_INSTRUCTION_METERING=1
fi

# Build with all available cores
make -j$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)

echo "WAMR built successfully at ${BUILD_DIR}/libiwasm.a"
