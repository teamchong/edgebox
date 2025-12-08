#!/bin/bash
# Build minimal WasmEdge (no LLVM AOT compiler) for fast cold start
# Builds both static (.a) and shared (.dylib) libraries
# Output: lib/libwasmedge-minimal.a (~3MB vs 153MB full)

set -e

cd "$(dirname "$0")/.."

WASMEDGE_DIR="vendor/wasmedge"

if [ ! -d "$WASMEDGE_DIR" ]; then
    echo "WasmEdge submodule not found. Run: git submodule update --init"
    exit 1
fi

cd "$WASMEDGE_DIR"

echo "Building minimal WasmEdge (no LLVM, static + shared)..."

cmake -Bbuild -GNinja \
    -DCMAKE_BUILD_TYPE=Release \
    -DWASMEDGE_USE_LLVM=OFF \
    -DWASMEDGE_BUILD_SHARED_LIB=ON \
    -DWASMEDGE_BUILD_STATIC_LIB=ON \
    -DWASMEDGE_BUILD_TOOLS=OFF \
    -DWASMEDGE_BUILD_PLUGINS=OFF \
    -DWASMEDGE_BUILD_TESTS=OFF \
    -DWASMEDGE_FORCE_DISABLE_LTO=ON \
    -DWASMEDGE_PLUGIN_WASI_CRYPTO=OFF \
    -DWASMEDGE_PLUGIN_WASI_NN=OFF \
    -DWASMEDGE_PLUGIN_WASI_LOGGING=OFF \
    -DWASMEDGE_PLUGIN_PROCESS=OFF \
    -DWASMEDGE_PLUGIN_TENSORFLOW=OFF \
    -DWASMEDGE_PLUGIN_TENSORFLOWLITE=OFF \
    -DWASMEDGE_PLUGIN_IMAGE=OFF \
    -DWASMEDGE_PLUGIN_STABLEDIFFUSION=OFF

ninja -C build

# Copy to standard location
mkdir -p ../../lib
if [[ "$OSTYPE" == "darwin"* ]]; then
    cp build/lib/api/libwasmedge.dylib ../../lib/libwasmedge-minimal.dylib
    cp build/lib/api/libwasmedge.a ../../lib/libwasmedge-minimal.a
    echo "Built:"
    ls -lh ../../lib/libwasmedge-minimal.*
else
    cp build/lib/api/libwasmedge.so ../../lib/libwasmedge-minimal.so
    cp build/lib/api/libwasmedge.a ../../lib/libwasmedge-minimal.a
    echo "Built:"
    ls -lh ../../lib/libwasmedge-minimal.*
fi
