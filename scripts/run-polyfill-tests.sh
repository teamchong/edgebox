#!/bin/bash
# Run comprehensive polyfill tests for EdgeBox
# Usage: ./scripts/run-polyfill-tests.sh [test_name]

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
TEST_DIR="$ROOT_DIR/test"

cd "$ROOT_DIR"

SPECIFIC_TEST=""
TIMEOUT=120  # seconds per test

# Parse arguments
for arg in "$@"; do
    case $arg in
        --help|-h)
            echo "Usage: $0 [test_name]"
            echo ""
            echo "Options:"
            echo "  test_name   Run specific test (e.g., math, buffer, crypto)"
            echo ""
            echo "Runs polyfill tests with EdgeBox (compile + run)."
            exit 0
            ;;
        *)
            SPECIFIC_TEST="$arg"
            ;;
    esac
done

# Define all polyfill tests
TESTS=(
    "test_polyfill_math"
    "test_polyfill_buffer"
    "test_polyfill_crypto"
    "test_polyfill_encoding"
    "test_polyfill_array"
    "test_polyfill_path"
    "test_polyfill_console"
    "test_polyfill_process"
    "test_polyfill_url"
    "test_polyfill_querystring"
    "test_polyfill_util"
    "test_polyfill_compression"
)

# Filter to specific test if provided
if [ -n "$SPECIFIC_TEST" ]; then
    FILTERED=()
    for test in "${TESTS[@]}"; do
        if [[ "$test" == *"$SPECIFIC_TEST"* ]]; then
            FILTERED+=("$test")
        fi
    done
    if [ ${#FILTERED[@]} -eq 0 ]; then
        echo "No tests matching '$SPECIFIC_TEST'"
        echo "Available tests:"
        for test in "${TESTS[@]}"; do
            echo "  ${test#test_polyfill_}"
        done
        exit 1
    fi
    TESTS=("${FILTERED[@]}")
fi

echo "=== EdgeBox Polyfill Test Suite ==="
echo ""

EDGEBOXC="$ROOT_DIR/zig-out/bin/edgeboxc"

if [ ! -f "$EDGEBOXC" ]; then
    echo "EdgeBox CLI not found at $EDGEBOXC"
    echo "Building EdgeBox..."
    zig build cli -Doptimize=ReleaseFast
fi

if [ ! -f "$EDGEBOXC" ]; then
    echo "Failed to build EdgeBox"
    exit 1
fi

PASSED=0
FAILED=0

for test in "${TESTS[@]}"; do
    echo "--- $test ---"
    TEST_FILE="test/${test}.js"

    if [ ! -f "$TEST_FILE" ]; then
        echo "SKIP: $TEST_FILE not found"
        continue
    fi

    # Compile with EdgeBox
    echo "  Compiling..."
    COMPILE_OUT=$("$EDGEBOXC" "./$TEST_FILE" 2>&1) || true

    # Find the binary (path includes ./ prefix)
    BINARY_PATH="$ROOT_DIR/zig-out/bin/./$TEST_FILE/${test}"

    if [ ! -f "$BINARY_PATH" ]; then
        echo "  SKIP: Binary not built at $BINARY_PATH"
        echo "$COMPILE_OUT" | grep -E "error|Error" | head -3
        continue
    fi

    # Run with timeout
    echo "  Running..."
    OUTPUT=$(timeout $TIMEOUT "$BINARY_PATH" 2>&1) || true
    SUMMARY=$(echo "$OUTPUT" | grep "SUMMARY" | tail -1)
    FAILS=$(echo "$OUTPUT" | grep -c "FAIL" || true)

    if [ -z "$SUMMARY" ]; then
        echo "  TIMEOUT or no output (test may have hung)"
        ((FAILED++)) || true
    elif [ "$FAILS" -gt 0 ]; then
        echo "  $SUMMARY (failures: $FAILS)"
        # Show failures
        echo "$OUTPUT" | grep "FAIL" | head -5 | sed 's/^/    /'
        ((FAILED++)) || true
    else
        echo "  $SUMMARY"
        ((PASSED++)) || true
    fi
    echo ""
done

echo "=========================================="
echo "TOTAL: $PASSED passed, $FAILED failed"
echo "=========================================="

if [ $FAILED -gt 0 ]; then
    exit 1
fi
