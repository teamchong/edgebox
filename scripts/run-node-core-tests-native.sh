#!/bin/bash
# Run Node.js Core Tests on Native Runtimes (Node.js/Bun)
# Usage: ./scripts/run-node-core-tests-native.sh <engine> <module> [mode]
#   engine: node or bun
#   module: buffer, fs, path, crypto, or all
#   mode: direct (default) or compile (for bun build --compile)

set -e

ENGINE="${1:-node}"
MODULE="${2:-all}"
MODE="${3:-direct}"  # 'direct' or 'compile'
RESULTS_FILE="node-test-results-${ENGINE}-${MODULE}.txt"

# Counters
PASSED=0
FAILED=0
SKIPPED=0

echo "=== Node.js Core Tests for $ENGINE (mode: $MODE) ===" > "$RESULTS_FILE"
echo "Module: $MODULE" >> "$RESULTS_FILE"
echo "Started: $(date)" >> "$RESULTS_FILE"
echo "" >> "$RESULTS_FILE"

# Function to run a single test
run_test() {
    local test_name="$1"
    local test_code="$2"

    # Create temp test file
    local test_file="/tmp/node-test-${test_name}.js"
    echo "$test_code" > "$test_file"

    local output
    local exit_code=0

    if [ "$MODE" = "compile" ] && [ "$ENGINE" = "bun" ]; then
        # Bun: compile to binary then run
        local binary_file="/tmp/node-test-${test_name}"
        if ! bun build --compile "$test_file" --outfile "$binary_file" > /dev/null 2>&1; then
            echo "✗ $test_name (compile failed)" >> "$RESULTS_FILE"
            FAILED=$((FAILED + 1))
            rm -f "$test_file" "$binary_file"
            return
        fi
        output=$(timeout 10s "$binary_file" 2>&1) || exit_code=$?
        rm -f "$binary_file"
    else
        # Direct execution (Node.js or Bun direct mode)
        output=$(timeout 10s $ENGINE "$test_file" 2>&1) || exit_code=$?
    fi

    if echo "$output" | grep -q "^SKIP:"; then
        echo "○ $test_name (skipped)" >> "$RESULTS_FILE"
        SKIPPED=$((SKIPPED + 1))
    elif echo "$output" | grep -q "PASS"; then
        echo "✓ $test_name" >> "$RESULTS_FILE"
        PASSED=$((PASSED + 1))
    elif [ $exit_code -eq 0 ] && ! echo "$output" | grep -qE "(FAIL|Error|assert)"; then
        echo "✓ $test_name" >> "$RESULTS_FILE"
        PASSED=$((PASSED + 1))
    else
        echo "✗ $test_name" >> "$RESULTS_FILE"
        echo "  Output: $(echo "$output" | head -2)" >> "$RESULTS_FILE"
        FAILED=$((FAILED + 1))
    fi

    # Cleanup
    rm -f "$test_file"
}

echo "Running $MODULE tests..."
echo ""

if [ "$MODULE" = "buffer" ] || [ "$MODULE" = "all" ]; then
    # Buffer tests
    run_test "buffer-alloc" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b = Buffer.alloc(10);
assert.strictEqual(b.length, 10);
for (let i = 0; i < 10; i++) assert.strictEqual(b[i], 0);
console.log("PASS: Buffer.alloc");
'

    run_test "buffer-alloc-fill" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b = Buffer.alloc(5, 42);
assert.strictEqual(b.length, 5);
for (let i = 0; i < 5; i++) assert.strictEqual(b[i], 42);
console.log("PASS: Buffer.alloc with fill");
'

    run_test "buffer-allocUnsafe" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b = Buffer.allocUnsafe(10);
assert.strictEqual(b.length, 10);
console.log("PASS: Buffer.allocUnsafe");
'

    run_test "buffer-from-string" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b = Buffer.from("hello");
assert.strictEqual(b.length, 5);
assert.strictEqual(b.toString(), "hello");
console.log("PASS: Buffer.from string");
'

    run_test "buffer-from-array" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b = Buffer.from([1, 2, 3, 4, 5]);
assert.strictEqual(b.length, 5);
assert.strictEqual(b[0], 1);
assert.strictEqual(b[4], 5);
console.log("PASS: Buffer.from array");
'

    run_test "buffer-from-arraybuffer" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const ab = new ArrayBuffer(16);
const view = new Uint8Array(ab);
view[0] = 42;
const b = Buffer.from(ab);
assert.strictEqual(b.length, 16);
assert.strictEqual(b[0], 42);
console.log("PASS: Buffer.from ArrayBuffer");
'

    run_test "buffer-concat" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b1 = Buffer.from([1, 2]);
const b2 = Buffer.from([3, 4]);
const b3 = Buffer.concat([b1, b2]);
assert.strictEqual(b3.length, 4);
assert.strictEqual(b3[0], 1);
assert.strictEqual(b3[3], 4);
console.log("PASS: Buffer.concat");
'

    run_test "buffer-slice" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b = Buffer.from([1, 2, 3, 4, 5]);
const s = b.slice(1, 4);
assert.strictEqual(s.length, 3);
assert.strictEqual(s[0], 2);
assert.strictEqual(s[2], 4);
console.log("PASS: Buffer.slice");
'

    run_test "buffer-copy" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b1 = Buffer.from([1, 2, 3]);
const b2 = Buffer.alloc(5);
b1.copy(b2, 1);
assert.strictEqual(b2[0], 0);
assert.strictEqual(b2[1], 1);
assert.strictEqual(b2[2], 2);
assert.strictEqual(b2[3], 3);
console.log("PASS: Buffer.copy");
'
fi

# Summary
echo "" >> "$RESULTS_FILE"
echo "=== Node.js $MODULE Tests Summary ===" >> "$RESULTS_FILE"
echo "Passed:  $PASSED" >> "$RESULTS_FILE"
echo "Failed:  $FAILED" >> "$RESULTS_FILE"
echo "Skipped: $SKIPPED" >> "$RESULTS_FILE"

TOTAL=$((PASSED + FAILED))
if [ $TOTAL -gt 0 ]; then
    PASS_RATE=$(awk "BEGIN {printf \"%.1f%%\", ($PASSED / $TOTAL) * 100}")
else
    PASS_RATE="N/A"
fi
echo "Pass Rate: $PASS_RATE" >> "$RESULTS_FILE"

# Also write machine-readable output
echo "" >> "$RESULTS_FILE"
echo "passed: $PASSED" >> "$RESULTS_FILE"
echo "failed: $FAILED" >> "$RESULTS_FILE"
echo "skipped: $SKIPPED" >> "$RESULTS_FILE"

echo ""
cat "$RESULTS_FILE"
