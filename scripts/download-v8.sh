#!/bin/bash
# Download rusty_v8 prebuilt static library for V8 embedding
#
# Usage: ./scripts/download-v8.sh [version]
#
# Downloads the appropriate prebuilt librusty_v8.a for your platform from:
# https://github.com/denoland/rusty_v8/releases
#
# Also fetches binding.cc (extern "C" functions) from the rusty_v8 source.

set -e

V8_VERSION="${1:-v146.8.0}"
VENDOR_DIR="vendor/v8"
REPO="denoland/rusty_v8"

# Detect platform → Rust target triple
OS=$(uname -s)
ARCH=$(uname -m)

case "$OS" in
    Darwin)
        case "$ARCH" in
            arm64|aarch64) TARGET="aarch64-apple-darwin" ;;
            x86_64)        TARGET="x86_64-apple-darwin" ;;
            *) echo "[v8] Unsupported macOS architecture: $ARCH"; exit 1 ;;
        esac
        ;;
    Linux)
        case "$ARCH" in
            aarch64)  TARGET="aarch64-unknown-linux-gnu" ;;
            x86_64)   TARGET="x86_64-unknown-linux-gnu" ;;
            *) echo "[v8] Unsupported Linux architecture: $ARCH"; exit 1 ;;
        esac
        ;;
    *) echo "[v8] Unsupported OS: $OS"; exit 1 ;;
esac

LIB_NAME="librusty_v8_release_${TARGET}.a"
DOWNLOAD_URL="https://github.com/${REPO}/releases/download/${V8_VERSION}/${LIB_NAME}.gz"

echo "[v8] rusty_v8 ${V8_VERSION} for ${TARGET}"
echo "[v8] URL: ${DOWNLOAD_URL}"

# Create vendor directory
mkdir -p "$VENDOR_DIR"

# Download prebuilt library if not already present or version changed
VERSION_FILE="${VENDOR_DIR}/.version"
if [ -f "$VERSION_FILE" ] && [ "$(cat "$VERSION_FILE")" = "${V8_VERSION}-${TARGET}" ] && [ -f "${VENDOR_DIR}/${LIB_NAME}" ]; then
    echo "[v8] Already downloaded ${V8_VERSION} for ${TARGET}, skipping"
else
    echo "[v8] Downloading ${LIB_NAME}.gz (~60MB)..."
    TEMP_GZ=$(mktemp --suffix=.gz)
    curl -fSL -o "$TEMP_GZ" "$DOWNLOAD_URL"

    echo "[v8] Decompressing..."
    gunzip -f "$TEMP_GZ"
    mv "${TEMP_GZ%.gz}" "${VENDOR_DIR}/${LIB_NAME}"

    echo "${V8_VERSION}-${TARGET}" > "$VERSION_FILE"
fi

# Download binding source files (extern "C" API)
for FILE in binding.cc binding.hpp; do
    SRC_URL="https://raw.githubusercontent.com/${REPO}/${V8_VERSION}/src/${FILE}"
    if [ ! -f "${VENDOR_DIR}/${FILE}" ] || [ ! -f "$VERSION_FILE" ] || [ "$(cat "$VERSION_FILE")" != "${V8_VERSION}-${TARGET}" ]; then
        echo "[v8] Fetching ${FILE}..."
        curl -fSL -o "${VENDOR_DIR}/${FILE}" "$SRC_URL"
    else
        echo "[v8] ${FILE} already present"
    fi
done

# Download support.h (rusty_v8 helper types used by binding.cc)
SUPPORT_URL="https://raw.githubusercontent.com/${REPO}/${V8_VERSION}/src/support.h"
if [ ! -f "${VENDOR_DIR}/support.h" ]; then
    echo "[v8] Fetching support.h..."
    curl -fSL -o "${VENDOR_DIR}/support.h" "$SUPPORT_URL"
else
    echo "[v8] support.h already present"
fi

# Download V8 include headers (needed for C++ bridge compilation)
# Extract V8 version from rusty_v8 version: v146.8.0 → V8 14.6.202.9
# The headers come from chromium's V8 repo via gitiles archive API.
V8_INCLUDE_DIR="${VENDOR_DIR}/include"
if [ ! -d "$V8_INCLUDE_DIR" ] || [ ! -f "${V8_INCLUDE_DIR}/v8.h" ]; then
    # Parse V8 engine version from the prebuilt library (if available) or use known mapping
    # rusty_v8 v146.8.0 = V8 14.6.202.9
    V8_ENGINE_VERSION="14.6.202.9"
    INCLUDE_URL="https://chromium.googlesource.com/v8/v8/+archive/${V8_ENGINE_VERSION}/include.tar.gz"
    echo "[v8] Downloading V8 ${V8_ENGINE_VERSION} include headers..."
    TEMP_TAR=$(mktemp --suffix=.tar.gz)
    curl -fSL -o "$TEMP_TAR" "$INCLUDE_URL"
    mkdir -p "$V8_INCLUDE_DIR"
    tar xzf "$TEMP_TAR" -C "$V8_INCLUDE_DIR"
    rm "$TEMP_TAR"
    echo "[v8] V8 headers installed ($(find "$V8_INCLUDE_DIR" -name '*.h' | wc -l) header files)"
else
    echo "[v8] V8 include headers already present"
fi

# Verify
echo ""
echo "[v8] Contents of ${VENDOR_DIR}:"
ls -lh "$VENDOR_DIR"

if [ -f "${VENDOR_DIR}/${LIB_NAME}" ]; then
    SIZE=$(du -h "${VENDOR_DIR}/${LIB_NAME}" | cut -f1)
    echo ""
    echo "[v8] Static library: ${LIB_NAME} (${SIZE})"

    # Quick sanity check: library should export v8 symbols
    SYMBOLS=$(nm "${VENDOR_DIR}/${LIB_NAME}" 2>/dev/null | grep -c "v8__" || true)
    echo "[v8] V8 binding symbols found: ${SYMBOLS}"
else
    echo "[v8] ERROR: ${LIB_NAME} not found after download"
    exit 1
fi

if [ -f "${VENDOR_DIR}/binding.cc" ]; then
    FUNCS=$(grep -c 'extern "C"' "${VENDOR_DIR}/binding.cc" 2>/dev/null || grep -c '^[a-z_]*v8__' "${VENDOR_DIR}/binding.cc" 2>/dev/null || echo "?")
    echo "[v8] binding.cc extern \"C\" functions: ${FUNCS}"
else
    echo "[v8] WARNING: binding.cc not found"
fi

echo ""
echo "[v8] Done! rusty_v8 ${V8_VERSION} installed to ${VENDOR_DIR}"
echo ""
echo "Build: zig build v8-test"
echo "Run:   ./zig-out/bin/edgebox-v8-test"
