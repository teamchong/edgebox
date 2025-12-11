#!/bin/bash
# Extract opcode definitions from QuickJS source
# Run this when QuickJS is updated to regenerate opcodes.zig

QUICKJS_DIR="${1:-../../vendor/quickjs-ng}"

echo "// Auto-generated from QuickJS - DO NOT EDIT"
echo "// Generated: $(date)"
echo "// Source: $QUICKJS_DIR/quickjs.c"
echo ""
echo "pub const BC_VERSION: u8 = $(grep '#define BC_VERSION' "$QUICKJS_DIR/quickjs.c" | awk '{print $3}');"
echo ""
echo "pub const Opcode = enum(u8) {"

# Extract opcodes from the DEF macro
grep -E '^\s+DEF\(|^\s+def\(' "$QUICKJS_DIR/quickjs-opcode.h" 2>/dev/null | \
    sed 's/.*DEF(\([^,]*\).*/    \1,/' | \
    sed 's/.*def(\([^,]*\).*/    \1,/'

echo "};"
