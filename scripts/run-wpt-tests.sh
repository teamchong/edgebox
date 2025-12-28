#!/bin/bash
# Run Web Platform Tests (WPT) on EdgeBox
# Usage: ./scripts/run-wpt-tests.sh <wpt-dir> <suite>
#
# This runs WPT tests for web-standard APIs:
# - url: URL/URLSearchParams parsing
# - encoding: TextEncoder/TextDecoder
# - streams: Web Streams API
# - abort: AbortController/AbortSignal

set -e

# Cross-platform timeout function (macOS doesn't have timeout by default)
run_with_timeout() {
    local timeout_seconds="$1"
    shift
    if command -v timeout &> /dev/null; then
        timeout "${timeout_seconds}s" "$@"
    elif command -v gtimeout &> /dev/null; then
        # GNU coreutils on macOS (via homebrew)
        gtimeout "${timeout_seconds}s" "$@"
    else
        # Fallback: run without timeout (perl-based alternative)
        perl -e 'alarm shift; exec @ARGV' "$timeout_seconds" "$@" 2>/dev/null || "$@"
    fi
}

WPT_DIR="${1:-wpt}"
SUITE="${2:-url}"

# Determine runtime and output file
if [ "$NODE_TEST" = "1" ]; then
    RUNTIME="node"
    RUNTIME_CMD="node"
    RESULTS_FILE="wpt-results-${SUITE}-node.txt"
elif [ "$BUN_TEST" = "1" ]; then
    RUNTIME="bun"
    RUNTIME_CMD="bun"
    RESULTS_FILE="wpt-results-${SUITE}-bun.txt"
else
    RUNTIME="edgebox"
    RUNTIME_CMD="./zig-out/bin/edgebox"
    RESULTS_FILE="wpt-results-${SUITE}.txt"
fi

# Counters
PASSED=0
FAILED=0
SKIPPED=0
TOTAL_COMPILE_TIME=0
TOTAL_RUN_TIME=0

# Get time in milliseconds (cross-platform)
get_time_ms() {
    if [[ "$OSTYPE" == "darwin"* ]]; then
        python3 -c 'import time; print(int(time.time() * 1000))'
    else
        echo $(($(date +%s%N) / 1000000))
    fi
}

echo "=== WPT Tests for $RUNTIME ===" > "$RESULTS_FILE"
echo "Suite: $SUITE" >> "$RESULTS_FILE"
echo "Started: $(date)" >> "$RESULTS_FILE"
echo "" >> "$RESULTS_FILE"

# Create a temp directory for test builds
TEST_BUILD_DIR="/tmp/edgebox-wpt-tests"
rm -rf "$TEST_BUILD_DIR"
mkdir -p "$TEST_BUILD_DIR"

# Function to run a single test
run_test() {
    local test_name="$1"
    local test_code="$2"

    # Create a test app directory
    local test_app_dir="$TEST_BUILD_DIR/$test_name"
    mkdir -p "$test_app_dir"

    # Write test code with print() polyfill for Node.js compatibility
    # (EdgeBox has print() built-in, Node.js needs console.log)
    cat > "$test_app_dir/index.js" << 'POLYFILL'
if (typeof print === 'undefined') { globalThis.print = console.log; }
POLYFILL
    echo "$test_code" >> "$test_app_dir/index.js"

    local output
    local exit_code=0
    local compile_time_ms=0
    local run_time_ms=0

    if [ "$RUNTIME" = "edgebox" ]; then
        # Compile first
        local compile_output
        local compile_exit=0
        local compile_start=$(get_time_ms)
        compile_output=$(./zig-out/bin/edgeboxc build "$test_app_dir" 2>&1) || compile_exit=$?
        local compile_end=$(get_time_ms)
        compile_time_ms=$((compile_end - compile_start))
        TOTAL_COMPILE_TIME=$((TOTAL_COMPILE_TIME + compile_time_ms))

        if [ $compile_exit -ne 0 ]; then
            echo "✗ $test_name (compile failed)" >> "$RESULTS_FILE"
            FAILED=$((FAILED + 1))
            rm -rf "$test_app_dir"
            return
        fi

        # Find compiled output
        local wasm_file="./zig-out/bin/tmp/edgebox-wpt-tests/$test_name/$test_name.aot"
        if [ ! -f "$wasm_file" ]; then
            wasm_file="./zig-out/bin/tmp/edgebox-wpt-tests/$test_name/$test_name.wasm"
        fi

        if [ ! -f "$wasm_file" ]; then
            echo "✗ $test_name (no wasm output)" >> "$RESULTS_FILE"
            FAILED=$((FAILED + 1))
            rm -rf "$test_app_dir"
            return
        fi

        local run_start=$(get_time_ms)
        output=$(run_with_timeout 30 ./zig-out/bin/edgebox "$wasm_file" 2>&1) || exit_code=$?
        local run_end=$(get_time_ms)
        run_time_ms=$((run_end - run_start))
        TOTAL_RUN_TIME=$((TOTAL_RUN_TIME + run_time_ms))
    else
        local run_start=$(get_time_ms)
        output=$(run_with_timeout 30 $RUNTIME_CMD "$test_app_dir/index.js" 2>&1) || exit_code=$?
        local run_end=$(get_time_ms)
        run_time_ms=$((run_end - run_start))
        TOTAL_RUN_TIME=$((TOTAL_RUN_TIME + run_time_ms))
    fi

    # Format timing for display
    local timing_info=""
    if [ "$RUNTIME" = "edgebox" ]; then
        timing_info=" (compile: ${compile_time_ms}ms, run: ${run_time_ms}ms)"
    else
        timing_info=" (${run_time_ms}ms)"
    fi

    if echo "$output" | grep -q "^SKIP:"; then
        echo "○ $test_name (skipped)" >> "$RESULTS_FILE"
        SKIPPED=$((SKIPPED + 1))
    elif echo "$output" | grep -q "PASS"; then
        echo "✓ $test_name$timing_info" >> "$RESULTS_FILE"
        PASSED=$((PASSED + 1))
    elif [ $exit_code -eq 0 ] && ! echo "$output" | grep -qE "(FAIL|Error|assert)"; then
        echo "✓ $test_name$timing_info" >> "$RESULTS_FILE"
        PASSED=$((PASSED + 1))
    else
        echo "✗ $test_name$timing_info" >> "$RESULTS_FILE"
        echo "  Output: $(echo "$output" | head -2)" >> "$RESULTS_FILE"
        FAILED=$((FAILED + 1))
    fi

    # Cleanup
    rm -rf "$test_app_dir"
    rm -rf "./zig-out/bin/tmp/edgebox-wpt-tests/$test_name.aot"
    rm -rf "./zig-out/bin/tmp/edgebox-wpt-tests/$test_name.wasm"
}

echo "Running $SUITE tests..."
echo ""

if [ "$SUITE" = "url" ] || [ "$SUITE" = "all" ]; then
    # URL parsing tests (based on WPT url/ tests)
    run_test "url-constructor" '
const assert = require("assert");
const url = new URL("https://example.com:8080/path?query=value#hash");
assert.strictEqual(url.protocol, "https:");
assert.strictEqual(url.hostname, "example.com");
assert.strictEqual(url.port, "8080");
assert.strictEqual(url.pathname, "/path");
assert.strictEqual(url.search, "?query=value");
assert.strictEqual(url.hash, "#hash");
print("PASS: URL constructor");
'

    run_test "url-origin" '
const assert = require("assert");
const url = new URL("https://example.com:8080/path");
assert.strictEqual(url.origin, "https://example.com:8080");
print("PASS: URL origin");
'

    run_test "url-searchparams" '
const assert = require("assert");
const params = new URLSearchParams("foo=1&bar=2");
assert.strictEqual(params.get("foo"), "1");
assert.strictEqual(params.get("bar"), "2");
assert.strictEqual(params.has("foo"), true);
assert.strictEqual(params.has("baz"), false);
print("PASS: URLSearchParams");
'

    run_test "url-searchparams-append" '
const assert = require("assert");
const params = new URLSearchParams();
params.append("a", "1");
params.append("b", "2");
params.append("a", "3");
assert.strictEqual(params.toString(), "a=1&b=2&a=3");
print("PASS: URLSearchParams append");
'

    run_test "url-searchparams-delete" '
const assert = require("assert");
const params = new URLSearchParams("a=1&b=2&a=3");
params.delete("a");
assert.strictEqual(params.toString(), "b=2");
print("PASS: URLSearchParams delete");
'

    run_test "url-searchparams-iteration" '
const assert = require("assert");
const params = new URLSearchParams("a=1&b=2");
const keys = [...params.keys()];
const values = [...params.values()];
assert.deepStrictEqual(keys, ["a", "b"]);
assert.deepStrictEqual(values, ["1", "2"]);
print("PASS: URLSearchParams iteration");
'

    run_test "url-relative" '
const assert = require("assert");
const url = new URL("/path", "https://example.com");
assert.strictEqual(url.href, "https://example.com/path");
print("PASS: URL relative");
'

    run_test "url-idna" '
const assert = require("assert");
// Basic IDNA test - hostname lowercasing
const url = new URL("https://EXAMPLE.COM/path");
assert.strictEqual(url.hostname, "example.com");
print("PASS: URL IDNA basic");
'

    run_test "url-setters" '
const assert = require("assert");
const url = new URL("https://example.com/path");
url.pathname = "/new-path";
assert.strictEqual(url.pathname, "/new-path");
url.search = "?new=query";
assert.strictEqual(url.search, "?new=query");
print("PASS: URL setters");
'

    run_test "url-special-chars" '
const assert = require("assert");
const params = new URLSearchParams();
params.set("key", "hello world");
assert.strictEqual(params.toString(), "key=hello+world");
print("PASS: URL special chars encoding");
'
fi

if [ "$SUITE" = "encoding" ] || [ "$SUITE" = "all" ]; then
    # TextEncoder/TextDecoder tests (based on WPT encoding/ tests)
    run_test "textencoder-basic" '
const assert = require("assert");
const encoder = new TextEncoder();
const encoded = encoder.encode("hello");
assert.strictEqual(encoded.length, 5);
assert.strictEqual(encoded[0], 104); // h
assert.strictEqual(encoded[4], 111); // o
print("PASS: TextEncoder basic");
'

    run_test "textdecoder-basic" '
const assert = require("assert");
const decoder = new TextDecoder();
const decoded = decoder.decode(new Uint8Array([104, 101, 108, 108, 111]));
assert.strictEqual(decoded, "hello");
print("PASS: TextDecoder basic");
'

    run_test "textencoder-utf8-multibyte" '
const assert = require("assert");
const encoder = new TextEncoder();
// Japanese: こんにちは (5 characters, 15 bytes in UTF-8)
const encoded = encoder.encode("こんにちは");
assert.strictEqual(encoded.length, 15);
print("PASS: TextEncoder UTF-8 multibyte");
'

    run_test "textdecoder-utf8-multibyte" '
const assert = require("assert");
const decoder = new TextDecoder();
// UTF-8 bytes for: こんにちは
const bytes = new Uint8Array([227, 129, 147, 227, 130, 147, 227, 129, 171, 227, 129, 161, 227, 129, 175]);
const decoded = decoder.decode(bytes);
assert.strictEqual(decoded, "こんにちは");
print("PASS: TextDecoder UTF-8 multibyte");
'

    run_test "textencoder-emoji" '
const assert = require("assert");
const encoder = new TextEncoder();
const encoded = encoder.encode("👋");
assert.strictEqual(encoded.length, 4); // UTF-8 encoding of emoji
print("PASS: TextEncoder emoji");
'

    run_test "textdecoder-stream" '
const assert = require("assert");
const decoder = new TextDecoder("utf-8", { stream: true });
// Split a multi-byte character across chunks
const part1 = decoder.decode(new Uint8Array([0xE4]), { stream: true });
const part2 = decoder.decode(new Uint8Array([0xB8]), { stream: true });
const part3 = decoder.decode(new Uint8Array([0xAD])); // 中
const result = part1 + part2 + part3;
assert.strictEqual(result, "中");
print("PASS: TextDecoder streaming");
'

    run_test "textencoder-encodeinto" '
const assert = require("assert");
const encoder = new TextEncoder();
const dest = new Uint8Array(10);
const result = encoder.encodeInto("hello", dest);
assert.strictEqual(result.read, 5);
assert.strictEqual(result.written, 5);
print("PASS: TextEncoder encodeInto");
'
fi

if [ "$SUITE" = "streams" ] || [ "$SUITE" = "all" ]; then
    # Web Streams API tests (based on WPT streams/ tests)
    run_test "readable-stream-basic" '
const assert = require("assert");
let pulled = false;
const stream = new ReadableStream({
    pull(controller) {
        if (!pulled) {
            controller.enqueue("chunk");
            pulled = true;
        } else {
            controller.close();
        }
    }
});
const reader = stream.getReader();
reader.read().then(result => {
    assert.strictEqual(result.value, "chunk");
    assert.strictEqual(result.done, false);
    return reader.read();
}).then(result => {
    assert.strictEqual(result.done, true);
    print("PASS: ReadableStream basic");
});
'

    run_test "writable-stream-basic" '
const assert = require("assert");
const chunks = [];
const stream = new WritableStream({
    write(chunk) {
        chunks.push(chunk);
    }
});
const writer = stream.getWriter();
writer.write("hello").then(() => {
    return writer.write("world");
}).then(() => {
    return writer.close();
}).then(() => {
    assert.deepStrictEqual(chunks, ["hello", "world"]);
    print("PASS: WritableStream basic");
});
'

    run_test "transform-stream-basic" '
const assert = require("assert");
const transform = new TransformStream({
    transform(chunk, controller) {
        controller.enqueue(chunk.toUpperCase());
    }
});
const reader = transform.readable.getReader();
const writer = transform.writable.getWriter();
writer.write("hello").then(() => {
    return reader.read();
}).then(result => {
    assert.strictEqual(result.value, "HELLO");
    print("PASS: TransformStream basic");
});
'

    run_test "readable-stream-from-array" '
const assert = require("assert");
const data = ["a", "b", "c"];
let index = 0;
const stream = new ReadableStream({
    pull(controller) {
        if (index < data.length) {
            controller.enqueue(data[index++]);
        } else {
            controller.close();
        }
    }
});
const reader = stream.getReader();
const results = [];
function readAll() {
    return reader.read().then(result => {
        if (result.done) {
            assert.deepStrictEqual(results, ["a", "b", "c"]);
            print("PASS: ReadableStream from array");
            return;
        }
        results.push(result.value);
        return readAll();
    });
}
readAll();
'

    run_test "readable-stream-cancel" '
const assert = require("assert");
let cancelled = false;
const stream = new ReadableStream({
    cancel(reason) {
        cancelled = true;
        assert.strictEqual(reason, "test reason");
    }
});
const reader = stream.getReader();
reader.cancel("test reason").then(() => {
    assert.strictEqual(cancelled, true);
    print("PASS: ReadableStream cancel");
});
'
fi

if [ "$SUITE" = "abort" ] || [ "$SUITE" = "all" ]; then
    # AbortController/AbortSignal tests (based on WPT dom/abort/ tests)
    run_test "abort-controller-basic" '
const assert = require("assert");
const controller = new AbortController();
const signal = controller.signal;
assert.strictEqual(signal.aborted, false);
controller.abort();
assert.strictEqual(signal.aborted, true);
print("PASS: AbortController basic");
'

    run_test "abort-signal-event" '
const assert = require("assert");
const controller = new AbortController();
const signal = controller.signal;
let eventFired = false;
signal.addEventListener("abort", () => {
    eventFired = true;
});
controller.abort();
assert.strictEqual(eventFired, true);
print("PASS: AbortSignal abort event");
'

    run_test "abort-signal-reason" '
const assert = require("assert");
const controller = new AbortController();
controller.abort("custom reason");
assert.strictEqual(controller.signal.reason, "custom reason");
print("PASS: AbortSignal reason");
'

    run_test "abort-signal-timeout" '
const assert = require("assert");
// AbortSignal.timeout creates a signal that aborts after specified ms
if (typeof AbortSignal.timeout !== "function") {
    print("SKIP: AbortSignal.timeout not supported");
} else {
    const signal = AbortSignal.timeout(100);
    assert.strictEqual(signal.aborted, false);
    setTimeout(() => {
        assert.strictEqual(signal.aborted, true);
        print("PASS: AbortSignal.timeout");
    }, 150);
}
'

    run_test "abort-signal-any" '
const assert = require("assert");
// AbortSignal.any creates a signal that aborts when any input signal aborts
if (typeof AbortSignal.any !== "function") {
    print("SKIP: AbortSignal.any not supported");
} else {
    const c1 = new AbortController();
    const c2 = new AbortController();
    const signal = AbortSignal.any([c1.signal, c2.signal]);
    assert.strictEqual(signal.aborted, false);
    c1.abort("first");
    assert.strictEqual(signal.aborted, true);
    assert.strictEqual(signal.reason, "first");
    print("PASS: AbortSignal.any");
}
'

    run_test "abort-throwifaborted" '
const assert = require("assert");
const controller = new AbortController();
// Should not throw before abort
try {
    controller.signal.throwIfAborted();
} catch (e) {
    assert.fail("Should not throw before abort");
}
controller.abort();
// Should throw after abort
try {
    controller.signal.throwIfAborted();
    assert.fail("Should have thrown");
} catch (e) {
    assert.strictEqual(e.name, "AbortError");
    print("PASS: AbortSignal.throwIfAborted");
}
'
fi

# Summary
echo "" >> "$RESULTS_FILE"
echo "=== Summary ===" >> "$RESULTS_FILE"
echo "passed: $PASSED" >> "$RESULTS_FILE"
echo "failed: $FAILED" >> "$RESULTS_FILE"
echo "skipped: $SKIPPED" >> "$RESULTS_FILE"
echo "total: $((PASSED + FAILED + SKIPPED))" >> "$RESULTS_FILE"
if [ "$RUNTIME" = "edgebox" ]; then
    echo "compile_time: ${TOTAL_COMPILE_TIME}ms" >> "$RESULTS_FILE"
fi
echo "run_time: ${TOTAL_RUN_TIME}ms" >> "$RESULTS_FILE"

echo ""
echo "=== WPT $SUITE Results ($RUNTIME) ==="
echo "Passed: $PASSED"
echo "Failed: $FAILED"
echo "Skipped: $SKIPPED"
if [ "$RUNTIME" = "edgebox" ]; then
    echo "Compile time: ${TOTAL_COMPILE_TIME}ms"
fi
echo "Run time: ${TOTAL_RUN_TIME}ms"
echo ""

# Cleanup
rm -rf "$TEST_BUILD_DIR"

# Exit with failure if any tests failed
if [ $FAILED -gt 0 ]; then
    exit 1
fi
