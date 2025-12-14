#!/bin/bash
# Build WAMR (WebAssembly Micro Runtime) for the current platform
# This script is used by CI and can be run locally
set -e

# Detect platform
if [[ "$OSTYPE" == "darwin"* ]]; then
    PLATFORM="darwin"
else
    PLATFORM="linux"
fi

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
BUILD_DIR="${REPO_ROOT}/vendor/wamr/product-mini/platforms/${PLATFORM}/build"

# Skip if already built
if [ -f "${BUILD_DIR}/libiwasm.a" ]; then
    echo "WAMR already built at ${BUILD_DIR}/libiwasm.a"
    exit 0
fi

echo "Building WAMR for ${PLATFORM}..."
mkdir -p "${BUILD_DIR}"
cd "${BUILD_DIR}"

# Configure: SIMD=ON, Fast JIT=OFF, Instruction Metering=ON
# See CLAUDE.md for rationale
cmake .. -DCMAKE_BUILD_TYPE=Release \
    -DWAMR_BUILD_FAST_JIT=0 \
    -DWAMR_BUILD_SIMD=1 \
    -DWAMR_ENABLE_INSTRUCTION_METERING=1

# Build with all available cores
make -j$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)

echo "WAMR built successfully at ${BUILD_DIR}/libiwasm.a"
