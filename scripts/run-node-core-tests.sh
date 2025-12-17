#!/bin/bash
# Run Node.js Core Tests on EdgeBox
# Usage: ./scripts/run-node-core-tests.sh <node-core-dir> <module>

set -e

NODE_DIR="${1:-node-core}"
MODULE="${2:-all}"
RESULTS_FILE="node-test-results-${MODULE}.txt"

# Counters
PASSED=0
FAILED=0
SKIPPED=0

echo "=== Node.js Core Tests for EdgeBox ===" > "$RESULTS_FILE"
echo "Module: $MODULE" >> "$RESULTS_FILE"
echo "Started: $(date)" >> "$RESULTS_FILE"
echo "" >> "$RESULTS_FILE"

# Function to run a single test
run_test() {
    local test_file="$1"
    local test_name=$(basename "$test_file")

    # Create a wrapper that loads the test in EdgeBox context
    local wrapper="/tmp/test_wrapper_$$.js"
    cat > "$wrapper" << 'EOF'
// Minimal Node.js test harness for EdgeBox
const assert = require('assert');

// Common test utilities
global.common = {
    mustCall: function(fn, exact) {
        let called = 0;
        return function(...args) {
            called++;
            if (exact !== undefined && called > exact) {
                throw new Error(`Function called ${called} times, expected ${exact}`);
            }
            return fn ? fn.apply(this, args) : undefined;
        };
    },
    mustNotCall: function(msg) {
        return () => { throw new Error(msg || 'Function should not be called'); };
    },
    skip: function(msg) {
        print(`SKIP: ${msg}`);
        process.exit(77); // Special exit code for skipped tests
    },
    hasCrypto: typeof crypto !== 'undefined',
    hasIntl: false,
    isWindows: false,
    isLinux: true,
    isPOSIX: true,
    platformTimeout: (ms) => ms * 2,
};

// Load the actual test
try {
    require(process.argv[2]);
    print('PASS');
    process.exit(0);
} catch (e) {
    print(`FAIL: ${e.message}`);
    if (e.stack) print(e.stack);
    process.exit(1);
}
EOF

    # Run the test with timeout
    local output
    local exit_code

    output=$(timeout 10s ./zig-out/bin/edgebox "$wrapper" "$test_file" 2>&1 || echo "EXIT_CODE=$?")
    exit_code=$?

    if echo "$output" | grep -q "^PASS"; then
        echo "✓ $test_name" >> "$RESULTS_FILE"
        PASSED=$((PASSED + 1))
    elif echo "$output" | grep -q "^SKIP"; then
        echo "○ $test_name (skipped)" >> "$RESULTS_FILE"
        SKIPPED=$((SKIPPED + 1))
    else
        echo "✗ $test_name" >> "$RESULTS_FILE"
        echo "  Error: $output" | head -3 >> "$RESULTS_FILE"
        FAILED=$((FAILED + 1))
    fi

    rm -f "$wrapper"
}

# Find and run tests based on module
if [ "$MODULE" = "all" ]; then
    TEST_PATTERN="test-*.js"
elif [ "$MODULE" = "buffer" ]; then
    TEST_PATTERN="test-buffer*.js"
elif [ "$MODULE" = "fs" ]; then
    TEST_PATTERN="test-fs*.js"
elif [ "$MODULE" = "path" ]; then
    TEST_PATTERN="test-path*.js"
elif [ "$MODULE" = "crypto" ]; then
    TEST_PATTERN="test-crypto*.js"
elif [ "$MODULE" = "timers" ]; then
    TEST_PATTERN="test-timers*.js"
elif [ "$MODULE" = "process" ]; then
    TEST_PATTERN="test-process*.js"
elif [ "$MODULE" = "util" ]; then
    TEST_PATTERN="test-util*.js"
elif [ "$MODULE" = "events" ]; then
    TEST_PATTERN="test-events*.js"
elif [ "$MODULE" = "stream" ]; then
    TEST_PATTERN="test-stream*.js"
else
    echo "Unknown module: $MODULE"
    echo "Valid modules: all, buffer, fs, path, crypto, timers, process, util, events, stream"
    exit 1
fi

echo "Running tests matching: $TEST_PATTERN"
echo ""

# Run all matching tests
for test_file in "$NODE_DIR"/test/parallel/$TEST_PATTERN; do
    [ -f "$test_file" ] || continue
    run_test "$test_file"
done

# Summary
echo "" >> "$RESULTS_FILE"
echo "=== Summary ===" >> "$RESULTS_FILE"
echo "passed: $PASSED" >> "$RESULTS_FILE"
echo "failed: $FAILED" >> "$RESULTS_FILE"
echo "skipped: $SKIPPED" >> "$RESULTS_FILE"

TOTAL=$((PASSED + FAILED))
if [ $TOTAL -gt 0 ]; then
    PASS_RATE=$(awk "BEGIN {printf \"%.1f%%\", ($PASSED / $TOTAL) * 100}")
else
    PASS_RATE="N/A"
fi

echo "pass_rate: $PASS_RATE" >> "$RESULTS_FILE"
echo "Ended: $(date)" >> "$RESULTS_FILE"

# Print summary to console
echo ""
echo "=== Node.js $MODULE Tests Summary ==="
echo "Passed:  $PASSED"
echo "Failed:  $FAILED"
echo "Skipped: $SKIPPED"
echo "Pass Rate: $PASS_RATE"

# Exit with error if any tests failed
[ $FAILED -eq 0 ]
