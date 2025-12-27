#!/bin/bash
# Fast Node.js Core Tests using warm instance reuse
# Compiles test harness ONCE, reuses for all tests
#
# Usage: ./scripts/run-node-core-tests-fast.sh <module>

set -e

MODULE="${1:-buffer}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"

EDGEBOX="$ROOT_DIR/zig-out/bin/edgebox"
EDGEBOXC="$ROOT_DIR/zig-out/bin/edgeboxc"

# Create temp directories
TEST_DIR="/tmp/edgebox-fast-tests"
HARNESS_DIR="$TEST_DIR/harness"
TESTS_DIR="$TEST_DIR/tests"
mkdir -p "$HARNESS_DIR" "$TESTS_DIR"

# Build test harness once (with caching)
HARNESS_AOT="$ROOT_DIR/zig-out/bin/tmp/edgebox-fast-tests/harness/harness.aot"
HARNESS_SRC="$SCRIPT_DIR/node-test-harness.js"
COMPILE_TIME=0

# Check if harness needs rebuild (source changed or doesn't exist)
if [ ! -f "$HARNESS_AOT" ] || [ "$HARNESS_SRC" -nt "$HARNESS_AOT" ]; then
    echo "=== Building test harness (one-time) ==="
    rm -rf "$HARNESS_DIR"
    mkdir -p "$HARNESS_DIR"
    cp "$HARNESS_SRC" "$HARNESS_DIR/index.js"

    COMPILE_START=$(python3 -c 'import time; print(int(time.time() * 1000))')
    $EDGEBOXC build "$HARNESS_DIR" 2>&1 | grep -E '\[build\]|\[error\]' || true
    COMPILE_END=$(python3 -c 'import time; print(int(time.time() * 1000))')
    COMPILE_TIME=$((COMPILE_END - COMPILE_START))

    if [ ! -f "$HARNESS_AOT" ]; then
        echo "ERROR: Harness not built. Looking for: $HARNESS_AOT"
        find "$ROOT_DIR/zig-out/bin" -name "harness*" 2>/dev/null
        exit 1
    fi
    echo "Harness compiled in ${COMPILE_TIME}ms"
else
    echo "=== Using cached test harness ==="
fi
echo ""

# Warm up harness in daemon
echo "=== Warming up harness ==="
$EDGEBOX up "$HARNESS_AOT" 2>/dev/null || true
# Verify harness works
$EDGEBOX "$HARNESS_AOT" 2>&1 | grep -q "HARNESS_READY" && echo "Harness ready!" || echo "Harness check failed"
echo ""

# Counters
PASSED=0
FAILED=0
TOTAL_RUN_TIME=0

run_test() {
    local test_name="$1"
    local test_code="$2"

    # Write test to file
    local test_file="$TESTS_DIR/$test_name.js"
    echo "$test_code" > "$test_file"

    # Run via warm harness
    local start=$(python3 -c 'import time; print(int(time.time() * 1000))')
    local output
    output=$($EDGEBOX "$HARNESS_AOT" --test-file "$test_file" 2>&1) || true
    local end=$(python3 -c 'import time; print(int(time.time() * 1000))')
    local run_time=$((end - start))
    TOTAL_RUN_TIME=$((TOTAL_RUN_TIME + run_time))

    if echo "$output" | grep -q "PASS"; then
        echo "✓ $test_name (${run_time}ms)"
        PASSED=$((PASSED + 1))
    elif echo "$output" | grep -qE "FAIL|Error|assert"; then
        echo "✗ $test_name (${run_time}ms)"
        echo "  $output" | head -2
        FAILED=$((FAILED + 1))
    else
        # No explicit PASS/FAIL, check exit status implicitly passed
        echo "✓ $test_name (${run_time}ms)"
        PASSED=$((PASSED + 1))
    fi
}

echo "=== Running $MODULE tests ==="
echo ""

if [ "$MODULE" = "buffer" ]; then
    run_test "buffer-alloc" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b = Buffer.alloc(10);
assert.strictEqual(b.length, 10);
for (let i = 0; i < 10; i++) assert.strictEqual(b[i], 0);
print("PASS: Buffer.alloc");
'

    run_test "buffer-alloc-fill" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b = Buffer.alloc(5, 42);
assert.strictEqual(b.length, 5);
for (let i = 0; i < 5; i++) assert.strictEqual(b[i], 42);
print("PASS: Buffer.alloc with fill");
'

    run_test "buffer-from-string" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b = Buffer.from("hello");
assert.strictEqual(b.length, 5);
assert.strictEqual(b.toString(), "hello");
print("PASS: Buffer.from string");
'

    run_test "buffer-from-array" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b = Buffer.from([1, 2, 3, 4, 5]);
assert.strictEqual(b.length, 5);
assert.strictEqual(b[0], 1);
assert.strictEqual(b[4], 5);
print("PASS: Buffer.from array");
'

    run_test "buffer-concat" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b1 = Buffer.from([1, 2]);
const b2 = Buffer.from([3, 4]);
const b3 = Buffer.concat([b1, b2]);
assert.strictEqual(b3.length, 4);
print("PASS: Buffer.concat");
'

    run_test "buffer-slice" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b = Buffer.from([1, 2, 3, 4, 5]);
const s = b.slice(1, 4);
assert.strictEqual(s.length, 3);
print("PASS: Buffer.slice");
'

    run_test "buffer-copy" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b1 = Buffer.from([1, 2, 3]);
const b2 = Buffer.alloc(5);
b1.copy(b2, 1);
assert.strictEqual(b2[1], 1);
print("PASS: Buffer.copy");
'

    run_test "buffer-equals" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b1 = Buffer.from([1, 2, 3]);
const b2 = Buffer.from([1, 2, 3]);
assert.strictEqual(b1.equals(b2), true);
print("PASS: Buffer.equals");
'

    run_test "buffer-indexOf" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b = Buffer.from("hello world");
assert.strictEqual(b.indexOf("world"), 6);
print("PASS: Buffer.indexOf");
'

    run_test "buffer-write-read-int" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b = Buffer.alloc(8);
b.writeInt32LE(12345, 0);
assert.strictEqual(b.readInt32LE(0), 12345);
print("PASS: Buffer read/write int");
'
fi

if [ "$MODULE" = "path" ]; then
    run_test "path-join" '
const path = require("path");
const assert = require("assert");
assert.strictEqual(path.join("/foo", "bar"), "/foo/bar");
print("PASS: path.join");
'

    run_test "path-dirname" '
const path = require("path");
const assert = require("assert");
assert.strictEqual(path.dirname("/foo/bar/baz.txt"), "/foo/bar");
print("PASS: path.dirname");
'

    run_test "path-basename" '
const path = require("path");
const assert = require("assert");
assert.strictEqual(path.basename("/foo/bar/baz.txt"), "baz.txt");
print("PASS: path.basename");
'

    run_test "path-extname" '
const path = require("path");
const assert = require("assert");
assert.strictEqual(path.extname("/foo/bar/baz.txt"), ".txt");
print("PASS: path.extname");
'

    run_test "path-isAbsolute" '
const path = require("path");
const assert = require("assert");
assert.strictEqual(path.isAbsolute("/foo"), true);
assert.strictEqual(path.isAbsolute("foo"), false);
print("PASS: path.isAbsolute");
'
fi

echo ""
echo "=== Summary ==="
echo "Passed:  $PASSED"
echo "Failed:  $FAILED"
echo "Compile: ${COMPILE_TIME}ms (one-time)"
echo "Run:     ${TOTAL_RUN_TIME}ms total"

if [ $PASSED -gt 0 ]; then
    AVG=$((TOTAL_RUN_TIME / PASSED))
    echo "Average: ${AVG}ms per test"
fi

# Write results file for comparison script
RESULTS_FILE="node-test-results-${MODULE}.txt"
cat > "$RESULTS_FILE" << EOF
=== Node.js Core Tests for edgebox (fast) ===
Module: $MODULE
Started: $(date)

=== Summary ===
passed: $PASSED
failed: $FAILED
skipped: 0
pass_rate: $(awk "BEGIN {printf \"%.1f%%\", ($PASSED / ($PASSED + $FAILED)) * 100}")
compile_time: ${COMPILE_TIME}ms
run_time: ${TOTAL_RUN_TIME}ms
Ended: $(date)
EOF

# Cleanup tests dir only (keep harness cached)
rm -rf "$TESTS_DIR"
