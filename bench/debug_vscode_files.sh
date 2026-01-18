#!/bin/bash
# Test vscode files one by one to find failing files
# Usage: START_IDX=640 END_IDX=700 ./bench/debug_vscode_files.sh

TSC_BIN="${TSC_BIN:-zig-out/bin/benchmark/node_modules/typescript/lib/tsc.js/tsc}"
VSCODE_SRC="benchmark/fixtures/vscode/src"
OUT_DIR="/tmp/vscode_debug_out"
START_IDX="${START_IDX:-640}"
END_IDX="${END_IDX:-700}"

# Create temp file with sorted file list
FILELIST=$(mktemp)
find "$VSCODE_SRC" -name "*.ts" ! -name "*.d.ts" | sort > "$FILELIST"
TOTAL=$(wc -l < "$FILELIST" | tr -d ' ')

echo "Testing files $START_IDX to $END_IDX (total: $TOTAL files)"
echo "TSC binary: $TSC_BIN"
echo "---"

FAIL_COUNT=0

idx=0
while IFS= read -r file; do
  if [ $idx -ge $START_IDX ] && [ $idx -le $END_IDX ]; then
    rm -rf "$OUT_DIR"
    mkdir -p "$OUT_DIR"

    # Get relative path for cleaner output
    rel_path="${file#$VSCODE_SRC/}"

    # Note: Don't suppress output - EdgeBox has a bug where output isn't emitted when stderr is redirected
    "$TSC_BIN" --outDir "$OUT_DIR" --target ES2020 --module ESNext \
      --moduleResolution node --skipLibCheck --noEmit false --declaration false \
      "$file"

    # Check if output was produced
    OUTPUT_COUNT=$(find "$OUT_DIR" -name "*.js" 2>/dev/null | wc -l | tr -d ' ')
    if [ "$OUTPUT_COUNT" -eq 0 ]; then
      echo "[$idx] FAIL: $rel_path"
      FAIL_COUNT=$((FAIL_COUNT + 1))
    else
      echo "[$idx] OK: $rel_path ($OUTPUT_COUNT files)"
    fi
  fi

  if [ $idx -gt $END_IDX ]; then
    break
  fi

  idx=$((idx + 1))
done < "$FILELIST"

rm -f "$FILELIST"

echo "---"
echo "Summary: $FAIL_COUNT failures"
