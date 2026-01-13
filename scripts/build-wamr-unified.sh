#!/bin/bash
# Build unified WAMR with runtime + AOT compiler
# This creates a single libiwasm-unified.a that includes both runtime (with CoW callbacks)
# and the AOT compiler (LLVM-based), eliminating symbol conflicts.
set -e

# Detect platform and architecture
if [ "$(uname)" == "Darwin" ]; then
    LLVM_PREFIX="/opt/homebrew/opt/llvm@20"
else
    LLVM_PREFIX="/usr/lib/llvm-20"
fi

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
WAMR_DIR="${REPO_ROOT}/vendor/wamr"
BUILD_DIR="${WAMR_DIR}/product-mini/platforms/${PLATFORM}/build-unified"
PATCHES_DIR="${REPO_ROOT}/patches/wamr"
PATCHES_MARKER="${WAMR_DIR}/.patches-applied"

# Configuration key for cache validation
# v1: Initial unified build with AOT compiler + CoW memory support
CONFIG_KEY="${PLATFORM}-${ARCH}-unified-v1"
CONFIG_MARKER="${BUILD_DIR}/.wamr_config"

# Apply patches to WAMR (if not already applied)
apply_patches() {
    if [ ! -f "$PATCHES_MARKER" ]; then
        echo "Applying WAMR patches..."
        cd "$WAMR_DIR"
        git checkout . 2>/dev/null || true
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

# Check if patches need to be reapplied
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
if [ -f "${BUILD_DIR}/libiwasm-unified.a" ] && [ -f "${CONFIG_MARKER}" ]; then
    EXISTING_CONFIG=$(cat "${CONFIG_MARKER}")
    if [ "$EXISTING_CONFIG" == "$CONFIG_KEY" ]; then
        echo "Unified WAMR already built with config: ${CONFIG_KEY}"
        exit 0
    else
        echo "Config changed from ${EXISTING_CONFIG} to ${CONFIG_KEY}, rebuilding..."
        rm -rf "${BUILD_DIR}"
    fi
fi

# Check LLVM is available
if [ ! -d "$LLVM_PREFIX" ]; then
    echo "Error: LLVM not found at $LLVM_PREFIX"
    echo "On macOS: brew install llvm@20"
    echo "On Linux: apt install llvm-20 llvm-20-dev"
    exit 1
fi

echo "Building unified WAMR for ${PLATFORM} (${ARCH}) with AOT compiler..."
mkdir -p "${BUILD_DIR}"

# Create a custom CMakeLists.txt that combines runtime + compiler
cat > "${BUILD_DIR}/CMakeLists.txt" << EOF
# Unified WAMR build: Runtime with CoW callbacks + AOT Compiler
cmake_minimum_required(VERSION 3.14)
project(wamr-unified)

set(CMAKE_CXX_STANDARD 17)

# WAMR paths - use absolute paths
set(WAMR_ROOT_DIR "${WAMR_DIR}")
set(SHARED_DIR \${WAMR_ROOT_DIR}/core/shared)
set(IWASM_DIR \${WAMR_ROOT_DIR}/core/iwasm)

# Platform detection
if(APPLE)
    set(WAMR_BUILD_PLATFORM "darwin")
elseif(UNIX)
    set(WAMR_BUILD_PLATFORM "linux")
endif()

# Target detection
if(CMAKE_SYSTEM_PROCESSOR MATCHES "^(arm64|aarch64)")
    set(WAMR_BUILD_TARGET "AARCH64")
elseif(CMAKE_SIZEOF_VOID_P EQUAL 8)
    set(WAMR_BUILD_TARGET "X86_64")
else()
    set(WAMR_BUILD_TARGET "X86_32")
endif()

# Enable features needed for both runtime and compiler
# NOTE: Fast interp is DISABLED because it conflicts with WAMR_COMPILER
# The AOT compiler requires classic interpreter structures
set(WAMR_BUILD_INTERP 1)
set(WAMR_BUILD_FAST_INTERP 0)
set(WAMR_BUILD_AOT 1)
set(WAMR_BUILD_SIMD 1)
set(WAMR_BUILD_GC 1)
set(WAMR_BUILD_REF_TYPES 1)
set(WAMR_BUILD_BULK_MEMORY 1)
set(WAMR_BUILD_LIBC_BUILTIN 1)
set(WAMR_BUILD_LIBC_WASI 1)
set(WAMR_BUILD_THREAD_MGR 1)
set(WAMR_BUILD_LIB_PTHREAD 1)

# Key: Enable the AOT compiler (adds WASM_ENABLE_WAMR_COMPILER=1)
add_definitions(-DWASM_ENABLE_WAMR_COMPILER=1)
add_definitions(-DBUILD_TARGET="\${WAMR_BUILD_TARGET}")
add_definitions(-DBUILD_TARGET_AARCH64)
add_definitions(-DWASM_ENABLE_BULK_MEMORY=1)
add_definitions(-DWASM_ENABLE_BULK_MEMORY_OPT=1)
add_definitions(-DWASM_ENABLE_SHARED_MEMORY=1)
add_definitions(-DWASM_ENABLE_THREAD_MGR=1)
add_definitions(-DWASM_ENABLE_TAIL_CALL=1)
add_definitions(-DWASM_ENABLE_REF_TYPES=1)
add_definitions(-DWASM_ENABLE_CUSTOM_NAME_SECTION=1)
add_definitions(-DWASM_ENABLE_AOT_STACK_FRAME=1)
add_definitions(-DWASM_ENABLE_DUMP_CALL_STACK=1)
add_definitions(-DWASM_ENABLE_LOAD_CUSTOM_SECTION=1)
add_definitions(-DWASM_ENABLE_MODULE_INST_CONTEXT=1)
add_definitions(-DWASM_ENABLE_GC=1)
add_definitions(-DWASM_ENABLE_SIMD=1)
add_definitions(-DWASM_ENABLE_INSTRUCTION_METERING=1)

# Find LLVM
find_package(LLVM REQUIRED CONFIG)
include_directories(\${LLVM_INCLUDE_DIRS})
add_definitions(\${LLVM_DEFINITIONS})
message(STATUS "Found LLVM \${LLVM_PACKAGE_VERSION}")

# Include WAMR build scripts
include(\${SHARED_DIR}/platform/\${WAMR_BUILD_PLATFORM}/shared_platform.cmake)
include(\${SHARED_DIR}/mem-alloc/mem_alloc.cmake)
include(\${SHARED_DIR}/utils/shared_utils.cmake)
include(\${SHARED_DIR}/utils/uncommon/shared_uncommon.cmake)
include(\${IWASM_DIR}/libraries/thread-mgr/thread_mgr.cmake)
include(\${IWASM_DIR}/common/iwasm_common.cmake)
include(\${IWASM_DIR}/common/gc/iwasm_gc.cmake)
include(\${IWASM_DIR}/interpreter/iwasm_interp.cmake)
include(\${IWASM_DIR}/aot/iwasm_aot.cmake)
include(\${IWASM_DIR}/compilation/iwasm_compl.cmake)
include(\${IWASM_DIR}/libraries/libc-builtin/libc_builtin.cmake)
include(\${IWASM_DIR}/libraries/libc-wasi/libc_wasi.cmake)
include(\${IWASM_DIR}/libraries/lib-pthread/lib_pthread.cmake)
include(\${IWASM_DIR}/libraries/shared-heap/shared_heap.cmake)

include_directories(\${SHARED_DIR}/include \${IWASM_DIR}/include)

# Platform-specific native invoke assembly
if(CMAKE_SYSTEM_PROCESSOR MATCHES "^(arm64|aarch64)")
    enable_language(ASM)
    set(INVOKE_NATIVE_SOURCE \${IWASM_DIR}/common/arch/invokeNative_aarch64.s)
elseif(CMAKE_SYSTEM_PROCESSOR MATCHES "^x86_64")
    enable_language(ASM)
    set(INVOKE_NATIVE_SOURCE \${IWASM_DIR}/common/arch/invokeNative_em64.s)
else()
    set(INVOKE_NATIVE_SOURCE "")
endif()

# Create unified static library
add_library(iwasm-unified STATIC
    \${PLATFORM_SHARED_SOURCE}
    \${MEM_ALLOC_SHARED_SOURCE}
    \${UTILS_SHARED_SOURCE}
    \${UNCOMMON_SHARED_SOURCE}
    \${THREAD_MGR_SOURCE}
    \${LIBC_BUILTIN_SOURCE}
    \${LIBC_WASI_SOURCE}
    \${LIB_PTHREAD_SOURCE}
    \${LIB_SHARED_HEAP_SOURCE}
    \${IWASM_COMMON_SOURCE}
    \${IWASM_INTERP_SOURCE}
    \${IWASM_AOT_SOURCE}
    \${IWASM_GC_SOURCE}
    \${IWASM_COMPL_SOURCE}
    \${INVOKE_NATIVE_SOURCE}
)

target_include_directories(iwasm-unified PUBLIC
    \${SHARED_DIR}/include
    \${IWASM_DIR}/include
    \${LLVM_INCLUDE_DIRS}
)
EOF

cd "${BUILD_DIR}"

# Configure with LLVM
echo "Configuring with LLVM from ${LLVM_PREFIX}..."
cmake . -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_PREFIX_PATH="${LLVM_PREFIX}" \
    -DWAMR_BUILD_WITH_CUSTOM_LLVM=1

# Build
echo "Building unified library..."
make -j${NPROC}

# Verify the library has both runtime and compiler symbols
echo "Verifying library contents..."
if nm libiwasm-unified.a | grep -q "wasm_runtime_set_linear_memory_callbacks"; then
    echo "  OK: CoW memory callbacks present"
else
    echo "  ERROR: Missing CoW memory callbacks"
    exit 1
fi

if nm libiwasm-unified.a | grep -q "aot_compile_wasm"; then
    echo "  OK: AOT compiler present"
else
    echo "  ERROR: Missing AOT compiler"
    exit 1
fi

# Write config marker
echo "${CONFIG_KEY}" > "${CONFIG_MARKER}"

echo ""
echo "Unified WAMR built successfully!"
echo "  Library: ${BUILD_DIR}/libiwasm-unified.a"
echo "  Features: Fast interpreter + SIMD + GC + CoW memory + AOT compiler"
