#!/bin/bash
# Build WAMR with Fast JIT for wizer (SIMD support)
# This is separate from the runtime WAMR which uses fast-interpreter
set -e

# Detect platform
if [[ "$OSTYPE" == "darwin"* ]]; then
    PLATFORM="darwin"
else
    PLATFORM="linux"
fi

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
BUILD_DIR="${REPO_ROOT}/vendor/wamr/product-mini/platforms/${PLATFORM}/build-jit"

# Skip if already built
if [ -f "${BUILD_DIR}/libiwasm.a" ]; then
    echo "WAMR JIT already built at ${BUILD_DIR}/libiwasm.a"
    exit 0
fi

echo "Building WAMR with Fast JIT for wizer (SIMD support)..."
mkdir -p "${BUILD_DIR}"
cd "${BUILD_DIR}"

# Configure: SIMD=ON, Fast JIT=ON (for full SIMD support in wizer)
cmake .. -DCMAKE_BUILD_TYPE=Release \
    -DWAMR_BUILD_FAST_JIT=1 \
    -DWAMR_BUILD_SIMD=1 \
    -DWAMR_BUILD_INSTRUCTION_METERING=0

# Build with all available cores
make -j$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)

echo "WAMR JIT built successfully at ${BUILD_DIR}/libiwasm.a"
