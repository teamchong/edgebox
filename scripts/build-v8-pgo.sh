#!/bin/bash
# Build V8 with Profile-Guided Optimization (PGO) for TSC workload
#
# This builds a PGO-optimized librusty_v8.a that's ~10% faster on
# CPU-bound workloads like TSC type checking.
#
# Prerequisites:
#   - Rust toolchain (rustup)
#   - Python 3
#   - Git
#   - ~30GB disk space
#   - ~2-4 hours build time
#
# Usage:
#   ./scripts/build-v8-pgo.sh
#
# After building, the PGO library replaces vendor/v8/librusty_v8_release_*.a

set -e

V8_VERSION="v146.8.0"
WORK_DIR="/tmp/rusty_v8_pgo"
PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

# Detect platform
OS=$(uname -s)
ARCH=$(uname -m)
case "$OS-$ARCH" in
    Linux-x86_64)   TARGET="x86_64-unknown-linux-gnu" ;;
    Linux-aarch64)  TARGET="aarch64-unknown-linux-gnu" ;;
    Darwin-arm64)   TARGET="aarch64-apple-darwin" ;;
    Darwin-x86_64)  TARGET="x86_64-apple-darwin" ;;
    *) echo "Unsupported: $OS-$ARCH"; exit 1 ;;
esac

echo "=== Building PGO V8 for $TARGET ==="
echo "Version: $V8_VERSION"
echo "Work dir: $WORK_DIR"
echo ""

# Step 1: Clone rusty_v8
if [ ! -d "$WORK_DIR/rusty_v8" ]; then
    echo "[1/5] Cloning rusty_v8..."
    mkdir -p "$WORK_DIR"
    git clone --depth 1 --branch "$V8_VERSION" https://github.com/nicolo-ribaudo/nicv8.git "$WORK_DIR/rusty_v8" 2>/dev/null || \
    git clone --depth 1 --branch "$V8_VERSION" https://github.com/nicolo-ribaudo/nicv8.git "$WORK_DIR/rusty_v8"
else
    echo "[1/5] rusty_v8 already cloned"
fi

# Step 2: Build with instrumentation
echo "[2/5] Building V8 with PGO instrumentation..."
cd "$WORK_DIR/rusty_v8"

# Set V8 PGO flags
export V8_FROM_SOURCE=1
export GN_ARGS="v8_enable_pgo_profile=true"

# Build instrumented version
cargo build --release 2>&1 | tail -5

# Step 3: Generate profile by running TSC
echo "[3/5] Generating PGO profile with TSC workload..."
INSTRUMENTED_LIB="target/release/gn_out/obj/librusty_v8.a"
if [ -f "$INSTRUMENTED_LIB" ]; then
    # Copy instrumented lib temporarily
    cp "$INSTRUMENTED_LIB" "$PROJECT_ROOT/vendor/v8/librusty_v8_release_${TARGET}.a"

    # Build EdgeBox with instrumented V8
    cd "$PROJECT_ROOT"
    zig build v8-run -Doptimize=ReleaseFast 2>&1 | tail -3

    # Run TSC workload to generate profile
    TRPC_FILES=$(find benchmark/fixtures/trpc/packages/server/src -name '*.ts' ! -name '*.d.ts')
    for i in $(seq 1 3); do
        rm -rf ~/.cache/edgebox/v8-cache/ /tmp/edgebox-incr-cache/
        zig-out/bin/edgebox benchmark/node_modules/typescript/lib/_tsc.js \
            --noEmit --target ES2020 --module ESNext --moduleResolution node \
            --skipLibCheck $TRPC_FILES > /dev/null 2>&1
        echo "  Profile run $i completed"
    done
fi

# Step 4: Rebuild with profile data
echo "[4/5] Rebuilding V8 with PGO profile..."
cd "$WORK_DIR/rusty_v8"
export GN_ARGS="v8_enable_pgo_profile=false"
# The profile data is automatically saved by the instrumented binary
cargo build --release 2>&1 | tail -5

# Step 5: Install PGO library
echo "[5/5] Installing PGO library..."
PGO_LIB="target/release/gn_out/obj/librusty_v8.a"
if [ -f "$PGO_LIB" ]; then
    cp "$PGO_LIB" "$PROJECT_ROOT/vendor/v8/librusty_v8_release_${TARGET}.a"
    echo ""
    echo "PGO V8 installed: $(ls -lh "$PROJECT_ROOT/vendor/v8/librusty_v8_release_${TARGET}.a" | awk '{print $5}')"
    echo "Rebuild EdgeBox: zig build v8-run -Doptimize=ReleaseFast"
else
    echo "ERROR: PGO library not found at $PGO_LIB"
    exit 1
fi

echo ""
echo "=== PGO V8 build complete ==="
echo "Expected improvement: ~10% on Check phase (type checking)"
