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
if [[ "$PLATFORM" == "linux" && "$ARCH" == "x86_64" ]]; then
    CONFIG_KEY="linux-x64-simd-native-v3"
else
    CONFIG_KEY="${PLATFORM}-${ARCH}-simd-simde-v3"
fi
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

# Write config marker for cache validation
echo "${CONFIG_KEY}" > "${CONFIG_MARKER}"

echo "WAMR built successfully at ${BUILD_DIR}/libiwasm.a (config: ${CONFIG_KEY})"
