#!/bin/bash
# Build WAMR AOT Compiler (requires LLVM)
# This builds libaotclib.a and libvmlib.a for AOT compilation
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
BUILD_DIR="${REPO_ROOT}/vendor/wamr/wamr-compiler/build"

# Skip if already built
if [ -f "${BUILD_DIR}/libaotclib.a" ] && [ -f "${BUILD_DIR}/libvmlib.a" ]; then
    echo "WAMR compiler already built"
    exit 0
fi

echo "Building WAMR AOT compiler..."
mkdir -p "${BUILD_DIR}"
cd "${BUILD_DIR}"

# Configure with LLVM support
if [[ "$OSTYPE" == "darwin"* ]]; then
    cmake .. -DCMAKE_BUILD_TYPE=Release \
        -DWAMR_BUILD_SIMD=1 \
        -DWAMR_BUILD_WITH_CUSTOM_LLVM=1 \
        -DCMAKE_PREFIX_PATH=/opt/homebrew/opt/llvm@20
else
    CC=clang-20 CXX=clang++-20 \
    CXXFLAGS="-stdlib=libc++" \
    LDFLAGS="-stdlib=libc++ -lc++abi" \
    cmake .. -DCMAKE_BUILD_TYPE=Release \
        -DWAMR_BUILD_SIMD=1 \
        -DWAMR_BUILD_WITH_CUSTOM_LLVM=1 \
        -DLLVM_DIR=/usr/lib/llvm-20/lib/cmake/llvm
fi

# Build with all available cores
make -j$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)

echo "WAMR compiler built successfully at ${BUILD_DIR}/libaotclib.a"
