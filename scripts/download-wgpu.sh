#!/bin/bash
# Download wgpu-native pre-built binaries for WebGPU support
#
# Usage: ./scripts/download-wgpu.sh
#
# Downloads the appropriate binary for your platform from:
# https://github.com/gfx-rs/wgpu-native/releases

set -e

WGPU_VERSION="v27.0.4.0"
VENDOR_DIR="vendor/wgpu-native"

# Detect platform
OS=$(uname -s)
ARCH=$(uname -m)

case "$OS" in
    Darwin)
        case "$ARCH" in
            arm64|aarch64)
                PLATFORM="macos-aarch64"
                ;;
            x86_64)
                PLATFORM="macos-x86_64"
                ;;
            *)
                echo "Unsupported macOS architecture: $ARCH"
                exit 1
                ;;
        esac
        ;;
    Linux)
        case "$ARCH" in
            aarch64)
                PLATFORM="linux-aarch64"
                ;;
            x86_64)
                PLATFORM="linux-x86_64"
                ;;
            *)
                echo "Unsupported Linux architecture: $ARCH"
                exit 1
                ;;
        esac
        ;;
    *)
        echo "Unsupported OS: $OS"
        exit 1
        ;;
esac

URL="https://github.com/gfx-rs/wgpu-native/releases/download/${WGPU_VERSION}/wgpu-${PLATFORM}-release.zip"

echo "[wgpu] Downloading wgpu-native ${WGPU_VERSION} for ${PLATFORM}..."
echo "[wgpu] URL: ${URL}"

# Create vendor directory
mkdir -p "$VENDOR_DIR"

# Download and extract
TEMP_ZIP=$(mktemp)
curl -L -o "$TEMP_ZIP" "$URL"

echo "[wgpu] Extracting to ${VENDOR_DIR}..."
unzip -o "$TEMP_ZIP" -d "$VENDOR_DIR"
rm "$TEMP_ZIP"

# Check what we got
echo "[wgpu] Contents:"
ls -la "$VENDOR_DIR"

# Verify headers exist
if [ -f "$VENDOR_DIR/webgpu.h" ] || [ -f "$VENDOR_DIR/include/webgpu.h" ]; then
    echo "[wgpu] Headers found!"
else
    echo "[wgpu] Warning: webgpu.h not found in expected location"
fi

# Verify library exists
if [ -f "$VENDOR_DIR/libwgpu_native.a" ] || [ -f "$VENDOR_DIR/lib/libwgpu_native.a" ]; then
    echo "[wgpu] Static library found!"
elif [ -f "$VENDOR_DIR/libwgpu_native.dylib" ] || [ -f "$VENDOR_DIR/libwgpu_native.so" ]; then
    echo "[wgpu] Dynamic library found!"
else
    echo "[wgpu] Warning: wgpu library not found in expected location"
fi

echo ""
echo "[wgpu] Done! wgpu-native ${WGPU_VERSION} installed to ${VENDOR_DIR}"
echo ""
echo "Next steps:"
echo "  1. Update build.zig to link wgpu-native"
echo "  2. Implement wgpu API calls in src/gpu_worker.zig"
echo "  3. Build: zig build gpu-worker"
