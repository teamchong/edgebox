#!/bin/bash
# Run Node.js Core Tests on Native Runtimes (Node.js/Bun) with Parallel Execution
# Usage: ./scripts/run-node-core-tests-native.sh <engine> <module> [mode] [threads]
#   engine: node or bun
#   module: buffer, fs, path, crypto, or all
#   mode: direct (default) or compile (for bun build --compile)
#   threads: 3 (default, GitHub Actions macos-latest has 3 cores)

set -e

ENGINE="${1:-node}"
MODULE="${2:-all}"
MODE="${3:-direct}"  # 'direct' or 'compile'
THREADS="${4:-3}"    # Default 3 threads
RESULTS_FILE="node-test-results-${ENGINE}-${MODULE}.txt"

echo "=== Node.js Core Tests for $ENGINE (mode: $MODE) ===" > "$RESULTS_FILE"
echo "Module: $MODULE" >> "$RESULTS_FILE"
echo "Threads: $THREADS" >> "$RESULTS_FILE"
echo "Started: $(date)" >> "$RESULTS_FILE"
echo "" >> "$RESULTS_FILE"

# Create a temp directory for results
TEST_BUILD_DIR="/tmp/edgebox-native-tests-$$"
rm -rf "$TEST_BUILD_DIR"
mkdir -p "$TEST_BUILD_DIR"

RESULTS_DIR="$TEST_BUILD_DIR/results"
mkdir -p "$RESULTS_DIR"

# Script to run a single test (called by xargs)
cat > "$TEST_BUILD_DIR/run_single_test.sh" << 'SCRIPT_EOF'
#!/bin/bash
set -e

# Parse input: test_name|||test_code
IFS='|||' read -r test_name test_code <<< "$1"

result_file="$RESULTS_DIR/$test_name.result"
test_file="$TEST_BUILD_DIR/$test_name.js"
echo "$test_code" > "$test_file"

output=""
exit_code=0

if [ "$MODE" = "compile" ] && [ "$ENGINE" = "bun" ]; then
    # Bun: compile to binary then run
    binary_file="$TEST_BUILD_DIR/$test_name-bin"
    if ! bun build --compile "$test_file" --outfile "$binary_file" > /dev/null 2>&1; then
        echo "FAILED|$test_name|compile failed|" > "$result_file"
        rm -f "$test_file" "$binary_file"
        exit 0
    fi
    output=$(timeout 10s "$binary_file" 2>&1) || exit_code=$?
    rm -f "$binary_file"
else
    # Direct execution (Node.js or Bun direct mode)
    output=$(timeout 10s $ENGINE "$test_file" 2>&1) || exit_code=$?
fi

if echo "$output" | grep -q "^SKIP:"; then
    echo "SKIPPED|$test_name||" > "$result_file"
elif echo "$output" | grep -q "PASS"; then
    echo "PASSED|$test_name||" > "$result_file"
elif [ ${exit_code:-0} -eq 0 ] && ! echo "$output" | grep -qE "(FAIL|Error|assert)"; then
    echo "PASSED|$test_name||" > "$result_file"
else
    echo "FAILED|$test_name||$(echo "$output" | head -2)" > "$result_file"
fi

# Cleanup
rm -f "$test_file"
SCRIPT_EOF

chmod +x "$TEST_BUILD_DIR/run_single_test.sh"

# Export variables for the test script
export TEST_BUILD_DIR RESULTS_DIR ENGINE MODE

# Create test queue file
TEST_QUEUE="$TEST_BUILD_DIR/test_queue.txt"
> "$TEST_QUEUE"

echo "Queueing $MODULE tests..."

if [ "$MODULE" = "buffer" ] || [ "$MODULE" = "all" ]; then
    # Buffer tests - using ||| as delimiter
    cat >> "$TEST_QUEUE" << 'EOF'
buffer-alloc|||const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.alloc(10); assert.strictEqual(b.length, 10); for (let i = 0; i < 10; i++) assert.strictEqual(b[i], 0); console.log("PASS: Buffer.alloc");
buffer-alloc-fill|||const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.alloc(5, 42); assert.strictEqual(b.length, 5); for (let i = 0; i < 5; i++) assert.strictEqual(b[i], 42); console.log("PASS: Buffer.alloc with fill");
buffer-allocUnsafe|||const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.allocUnsafe(10); assert.strictEqual(b.length, 10); console.log("PASS: Buffer.allocUnsafe");
buffer-from-string|||const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.from("hello"); assert.strictEqual(b.length, 5); assert.strictEqual(b.toString(), "hello"); console.log("PASS: Buffer.from string");
buffer-from-array|||const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.from([1, 2, 3, 4, 5]); assert.strictEqual(b.length, 5); assert.strictEqual(b[0], 1); assert.strictEqual(b[4], 5); console.log("PASS: Buffer.from array");
buffer-from-arraybuffer|||const Buffer = require("buffer").Buffer; const assert = require("assert"); const ab = new ArrayBuffer(16); const view = new Uint8Array(ab); view[0] = 42; const b = Buffer.from(ab); assert.strictEqual(b.length, 16); assert.strictEqual(b[0], 42); console.log("PASS: Buffer.from ArrayBuffer");
buffer-concat|||const Buffer = require("buffer").Buffer; const assert = require("assert"); const b1 = Buffer.from([1, 2]); const b2 = Buffer.from([3, 4]); const b3 = Buffer.concat([b1, b2]); assert.strictEqual(b3.length, 4); assert.strictEqual(b3[0], 1); assert.strictEqual(b3[3], 4); console.log("PASS: Buffer.concat");
buffer-slice|||const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.from([1, 2, 3, 4, 5]); const s = b.slice(1, 4); assert.strictEqual(s.length, 3); assert.strictEqual(s[0], 2); assert.strictEqual(s[2], 4); console.log("PASS: Buffer.slice");
buffer-copy|||const Buffer = require("buffer").Buffer; const assert = require("assert"); const b1 = Buffer.from([1, 2, 3]); const b2 = Buffer.alloc(5); b1.copy(b2, 1); assert.strictEqual(b2[0], 0); assert.strictEqual(b2[1], 1); assert.strictEqual(b2[2], 2); assert.strictEqual(b2[3], 3); console.log("PASS: Buffer.copy");
EOF
fi

# Count total tests
TOTAL_TESTS=$(wc -l < "$TEST_QUEUE")
echo "Running $TOTAL_TESTS tests in parallel with $THREADS threads..."
echo ""

# Run tests in parallel using xargs with line-by-line processing
# Use -L 1 to process one line at a time, avoiding ARG_MAX limits
xargs -P "$THREADS" -L 1 "$TEST_BUILD_DIR/run_single_test.sh" < "$TEST_QUEUE"

# Collect results
PASSED=0
FAILED=0
SKIPPED=0

for result_file in "$RESULTS_DIR"/*.result; do
    if [ ! -f "$result_file" ]; then
        continue
    fi

    IFS='|' read -r status test_name error output < "$result_file"

    case "$status" in
        PASSED)
            echo "✓ $test_name" >> "$RESULTS_FILE"
            PASSED=$((PASSED + 1))
            ;;
        FAILED)
            echo "✗ $test_name${error:+ ($error)}" >> "$RESULTS_FILE"
            if [ -n "$output" ]; then
                echo "  Output: $output" >> "$RESULTS_FILE"
            fi
            FAILED=$((FAILED + 1))
            ;;
        SKIPPED)
            echo "○ $test_name (skipped)" >> "$RESULTS_FILE"
            SKIPPED=$((SKIPPED + 1))
            ;;
    esac
done

# Cleanup temp dir
rm -rf "$TEST_BUILD_DIR"

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
