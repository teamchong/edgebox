#!/bin/bash
# Run comprehensive polyfill tests for EdgeBox
# Usage: ./scripts/run-polyfill-tests.sh [--bun] [--node] [--edgebox] [test_name]

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
TEST_DIR="$ROOT_DIR/test"

cd "$ROOT_DIR"

# Default options
RUN_BUN=false
RUN_NODE=false
RUN_EDGEBOX=false
SPECIFIC_TEST=""
TIMEOUT=120  # seconds per test (some EdgeBox tests are slow)

# Parse arguments
for arg in "$@"; do
    case $arg in
        --bun)
            RUN_BUN=true
            ;;
        --node)
            RUN_NODE=true
            ;;
        --edgebox)
            RUN_EDGEBOX=true
            ;;
        --all)
            RUN_BUN=true
            RUN_NODE=true
            RUN_EDGEBOX=true
            ;;
        --help|-h)
            echo "Usage: $0 [--bun] [--node] [--edgebox] [--all] [test_name]"
            echo ""
            echo "Options:"
            echo "  --bun       Run tests with Bun"
            echo "  --node      Run tests with Node.js"
            echo "  --edgebox   Run tests with EdgeBox (compile + run)"
            echo "  --all       Run with all runtimes"
            echo "  test_name   Run specific test (e.g., math, buffer, crypto)"
            echo ""
            echo "If no runtime specified, defaults to Bun."
            exit 0
            ;;
        *)
            SPECIFIC_TEST="$arg"
            ;;
    esac
done

# Default to Bun if no runtime specified
if ! $RUN_BUN && ! $RUN_NODE && ! $RUN_EDGEBOX; then
    RUN_BUN=true
fi

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

TOTAL_PASSED=0
TOTAL_FAILED=0

# Run with Bun
if $RUN_BUN; then
    echo "=== Running with Bun ==="
    echo ""

    if ! command -v bun &> /dev/null; then
        echo "Bun not found, skipping Bun tests"
    else
        BUN_PASSED=0
        BUN_FAILED=0

        for test in "${TESTS[@]}"; do
            echo "--- $test ---"
            TEST_FILE="$TEST_DIR/${test}.js"

            if [ ! -f "$TEST_FILE" ]; then
                echo "SKIP: $TEST_FILE not found"
                continue
            fi

            OUTPUT=$(timeout $TIMEOUT bun run "$TEST_FILE" 2>&1) || true
            SUMMARY=$(echo "$OUTPUT" | grep "SUMMARY" | tail -1)
            FAILS=$(echo "$OUTPUT" | grep -c "FAIL" || true)

            if [ "$FAILS" -gt 0 ]; then
                echo "$SUMMARY (failures: $FAILS)"
                ((BUN_FAILED++)) || true
            else
                echo "$SUMMARY"
                ((BUN_PASSED++)) || true
            fi
            echo ""
        done

        echo "Bun Summary: $BUN_PASSED passed, $BUN_FAILED failed"
        TOTAL_PASSED=$((TOTAL_PASSED + BUN_PASSED))
        TOTAL_FAILED=$((TOTAL_FAILED + BUN_FAILED))
        echo ""
    fi
fi

# Run with Node.js
if $RUN_NODE; then
    echo "=== Running with Node.js ==="
    echo ""

    if ! command -v node &> /dev/null; then
        echo "Node.js not found, skipping Node tests"
    else
        NODE_PASSED=0
        NODE_FAILED=0

        for test in "${TESTS[@]}"; do
            echo "--- $test ---"
            TEST_FILE="$TEST_DIR/${test}.js"

            if [ ! -f "$TEST_FILE" ]; then
                echo "SKIP: $TEST_FILE not found"
                continue
            fi

            OUTPUT=$(timeout $TIMEOUT node "$TEST_FILE" 2>&1) || true
            SUMMARY=$(echo "$OUTPUT" | grep "SUMMARY" | tail -1)
            FAILS=$(echo "$OUTPUT" | grep -c "FAIL" || true)

            if [ "$FAILS" -gt 0 ]; then
                echo "$SUMMARY (failures: $FAILS)"
                ((NODE_FAILED++)) || true
            else
                echo "$SUMMARY"
                ((NODE_PASSED++)) || true
            fi
            echo ""
        done

        echo "Node.js Summary: $NODE_PASSED passed, $NODE_FAILED failed"
        TOTAL_PASSED=$((TOTAL_PASSED + NODE_PASSED))
        TOTAL_FAILED=$((TOTAL_FAILED + NODE_FAILED))
        echo ""
    fi
fi

# Run with EdgeBox
if $RUN_EDGEBOX; then
    echo "=== Running with EdgeBox ==="
    echo ""

    EDGEBOXC="$ROOT_DIR/zig-out/bin/edgeboxc"

    if [ ! -f "$EDGEBOXC" ]; then
        echo "EdgeBox CLI not found at $EDGEBOXC"
        echo "Building EdgeBox..."
        zig build cli -Doptimize=ReleaseFast
    fi

    if [ ! -f "$EDGEBOXC" ]; then
        echo "Failed to build EdgeBox, skipping EdgeBox tests"
    else
        EDGEBOX_PASSED=0
        EDGEBOX_FAILED=0

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
                ((EDGEBOX_FAILED++)) || true
            elif [ "$FAILS" -gt 0 ]; then
                echo "  $SUMMARY (failures: $FAILS)"
                # Show failures
                echo "$OUTPUT" | grep "FAIL" | head -5 | sed 's/^/    /'
                ((EDGEBOX_FAILED++)) || true
            else
                echo "  $SUMMARY"
                ((EDGEBOX_PASSED++)) || true
            fi
            echo ""
        done

        echo "EdgeBox Summary: $EDGEBOX_PASSED passed, $EDGEBOX_FAILED failed"
        TOTAL_PASSED=$((TOTAL_PASSED + EDGEBOX_PASSED))
        TOTAL_FAILED=$((TOTAL_FAILED + EDGEBOX_FAILED))
        echo ""
    fi
fi

# Final summary
echo "=========================================="
echo "TOTAL: $TOTAL_PASSED passed, $TOTAL_FAILED failed"
echo "=========================================="

if [ $TOTAL_FAILED -gt 0 ]; then
    exit 1
fi
