#!/usr/bin/env bash
set -e

# Compile edgebox-base.wasm to edgebox-base.aot
# This uses edgeboxc's embedded AOT compiler (WAMR + LLVM)

WASM_FILE="zig-out/bin/edgebox-base.wasm"
AOT_FILE="zig-out/bin/edgebox-base.aot"

if [ ! -f "$WASM_FILE" ]; then
    echo "Error: $WASM_FILE not found"
    echo "Run: zig build wasm"
    exit 1
fi

echo "Compiling $WASM_FILE to $AOT_FILE..."

# Create a temporary directory for the build
TEMP_DIR=$(mktemp -d)
trap "rm -rf $TEMP_DIR" EXIT

# Copy the WASM file to a temporary app directory
mkdir -p "$TEMP_DIR/app"
cp "$WASM_FILE" "$TEMP_DIR/app/app.wasm"

# Compile using edgeboxc's AOT compiler
# The build process will generate app.aot from app.wasm
./zig-out/bin/edgeboxc build "$TEMP_DIR/app" --force

# Copy the AOT file to the target location
if [ -f "zig-out/bin/app/app.aot" ]; then
    cp "zig-out/bin/app/app.aot" "$AOT_FILE"
    echo "AOT compilation complete: $AOT_FILE"
    ls -lh "$AOT_FILE"
else
    echo "Error: AOT compilation failed"
    exit 1
fi
