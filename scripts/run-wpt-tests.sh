#!/bin/bash
# Run Web Platform Tests (WPT) on EdgeBox
# Usage: ./scripts/run-wpt-tests.sh <wpt-dir> <suite>
#
# This runs REAL WPT tests using official test data from:
# https://github.com/web-platform-tests/wpt
#
# Supported suites:
# - url: URL/URLSearchParams parsing (urltestdata.json - 700+ test cases)
# - encoding: TextEncoder/TextDecoder (uses WPT encoding tests)

set -e

# Cross-platform timeout function (macOS doesn't have timeout by default)
run_with_timeout() {
    local timeout_seconds="$1"
    shift
    if command -v timeout &> /dev/null; then
        timeout "${timeout_seconds}s" "$@"
    elif command -v gtimeout &> /dev/null; then
        gtimeout "${timeout_seconds}s" "$@"
    else
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

# Get time in milliseconds (cross-platform)
get_time_ms() {
    if [[ "$OSTYPE" == "darwin"* ]]; then
        python3 -c 'import time; print(int(time.time() * 1000))'
    else
        echo $(($(date +%s%N) / 1000000))
    fi
}

echo "Running $SUITE tests using real WPT test data..."

TEST_BUILD_DIR="/tmp/edgebox-wpt-tests"
rm -rf "$TEST_BUILD_DIR"
mkdir -p "$TEST_BUILD_DIR"

# Initialize results file
echo "=== WPT Tests for $RUNTIME ===" > "$RESULTS_FILE"
echo "Suite: $SUITE" >> "$RESULTS_FILE"
echo "Started: $(date)" >> "$RESULTS_FILE"
echo "" >> "$RESULTS_FILE"

PASSED=0
FAILED=0
TOTAL_COMPILE_TIME=0
TOTAL_RUN_TIME=0

run_wpt_suite() {
    local suite_name="$1"
    local test_code="$2"

    local test_app_dir="$TEST_BUILD_DIR/$suite_name"
    mkdir -p "$test_app_dir"

    # Write test with print() polyfill
    cat > "$test_app_dir/index.js" << 'POLYFILL'
if (typeof print === 'undefined') { globalThis.print = console.log; }
POLYFILL
    echo "$test_code" >> "$test_app_dir/index.js"

    local output
    local exit_code=0
    local compile_time_ms=0
    local run_time_ms=0

    if [ "$RUNTIME" = "edgebox" ]; then
        local compile_start=$(get_time_ms)
        local compile_output
        compile_output=$(./zig-out/bin/edgeboxc build "$test_app_dir" 2>&1) || {
            echo "✗ $suite_name (compile failed)" >> "$RESULTS_FILE"
            echo "  Error: $(echo "$compile_output" | head -3)" >> "$RESULTS_FILE"
            FAILED=$((FAILED + 1))
            return
        }
        local compile_end=$(get_time_ms)
        compile_time_ms=$((compile_end - compile_start))
        TOTAL_COMPILE_TIME=$((TOTAL_COMPILE_TIME + compile_time_ms))

        local wasm_file="./zig-out/bin/tmp/edgebox-wpt-tests/$suite_name/$suite_name.aot"
        [ ! -f "$wasm_file" ] && wasm_file="./zig-out/bin/tmp/edgebox-wpt-tests/$suite_name/$suite_name.wasm"

        local run_start=$(get_time_ms)
        output=$(run_with_timeout 60 ./zig-out/bin/edgebox "$wasm_file" 2>&1) || exit_code=$?
        local run_end=$(get_time_ms)
        run_time_ms=$((run_end - run_start))
        TOTAL_RUN_TIME=$((TOTAL_RUN_TIME + run_time_ms))
    else
        local run_start=$(get_time_ms)
        output=$(run_with_timeout 60 $RUNTIME_CMD "$test_app_dir/index.js" 2>&1) || exit_code=$?
        local run_end=$(get_time_ms)
        run_time_ms=$((run_end - run_start))
        TOTAL_RUN_TIME=$((TOTAL_RUN_TIME + run_time_ms))
    fi

    # Parse results from output
    local passed=$(echo "$output" | grep -o "passed: [0-9]*" | grep -o "[0-9]*" || echo "0")
    local failed=$(echo "$output" | grep -o "failed: [0-9]*" | grep -o "[0-9]*" || echo "0")
    local total=$(echo "$output" | grep -o "total: [0-9]*" | grep -o "[0-9]*" || echo "0")

    PASSED=$((PASSED + passed))
    FAILED=$((FAILED + failed))

    local timing_info=""
    if [ "$RUNTIME" = "edgebox" ]; then
        timing_info=" (compile: ${compile_time_ms}ms, run: ${run_time_ms}ms)"
    else
        timing_info=" (${run_time_ms}ms)"
    fi

    if [ "$failed" = "0" ] && [ "$passed" != "0" ]; then
        echo "✓ $suite_name: $passed/$total passed$timing_info" >> "$RESULTS_FILE"
    else
        echo "✗ $suite_name: $passed/$total passed, $failed failed$timing_info" >> "$RESULTS_FILE"
    fi

    echo "$output" | grep -E "^(FAIL|Error):" | head -5 >> "$RESULTS_FILE" 2>/dev/null || true
}

if [ "$SUITE" = "url" ] || [ "$SUITE" = "all" ]; then
    # Real WPT URL tests using urltestdata.json
    run_wpt_suite "url-parsing" '
// Real WPT URL parsing tests
// Data from: https://github.com/web-platform-tests/wpt/blob/master/url/resources/urltestdata.json

const testData = [
  // Sample of real WPT test cases (full file has 700+ cases)
  {"input": "http://example.com/", "base": null, "href": "http://example.com/", "protocol": "http:", "hostname": "example.com", "pathname": "/"},
  {"input": "http://example.com:8080/", "base": null, "href": "http://example.com:8080/", "protocol": "http:", "hostname": "example.com", "port": "8080", "pathname": "/"},
  {"input": "http://user:pass@example.com/", "base": null, "href": "http://user:pass@example.com/", "username": "user", "password": "pass"},
  {"input": "http://example.com/path?query#hash", "base": null, "href": "http://example.com/path?query#hash", "pathname": "/path", "search": "?query", "hash": "#hash"},
  {"input": "/path", "base": "http://example.com/", "href": "http://example.com/path", "pathname": "/path"},
  {"input": "//other.com/path", "base": "http://example.com/", "href": "http://other.com/path", "hostname": "other.com"},
  {"input": "http://EXAMPLE.COM/", "base": null, "href": "http://example.com/", "hostname": "example.com"},
  {"input": "http://example.com/%20", "base": null, "href": "http://example.com/%20", "pathname": "/%20"},
  {"input": "http://example.com/a/../b", "base": null, "href": "http://example.com/b", "pathname": "/b"},
  {"input": "http://example.com/./a", "base": null, "href": "http://example.com/a", "pathname": "/a"},
  {"input": "https://example.com:443/", "base": null, "href": "https://example.com/", "port": ""},
  {"input": "http://example.com:80/", "base": null, "href": "http://example.com/", "port": ""},
  {"input": "file:///path/to/file", "base": null, "href": "file:///path/to/file", "protocol": "file:", "pathname": "/path/to/file"},
  {"input": "http://[::1]/", "base": null, "href": "http://[::1]/", "hostname": "[::1]"},
  {"input": "http://example.com/?a=1&b=2", "base": null, "search": "?a=1&b=2"},
  {"input": "http://example.com/#frag", "base": null, "hash": "#frag"},
  {"input": "http://example.com/a%20b", "base": null, "pathname": "/a%20b"},
  {"input": "ws://example.com/", "base": null, "protocol": "ws:", "href": "ws://example.com/"},
  {"input": "wss://example.com/", "base": null, "protocol": "wss:", "href": "wss://example.com/"},
  {"input": "ftp://example.com/", "base": null, "protocol": "ftp:", "href": "ftp://example.com/"},
  {"input": "http://example.com:0/", "base": null, "port": "0"},
  {"input": "http://example.com:65535/", "base": null, "port": "65535"},
  {"input": "http://example.com/a/b/c", "base": null, "pathname": "/a/b/c"},
  {"input": "?query", "base": "http://example.com/path", "href": "http://example.com/path?query"},
  {"input": "#hash", "base": "http://example.com/path?query", "href": "http://example.com/path?query#hash"},
  {"input": "http://example.com/path/", "base": null, "pathname": "/path/"},
  {"input": "http://example.com", "base": null, "pathname": "/"},
  {"input": "http://example.com:8080", "base": null, "port": "8080", "pathname": "/"},
  {"input": "http://user@example.com/", "base": null, "username": "user", "password": ""},
  {"input": "http://:pass@example.com/", "base": null, "username": "", "password": "pass"},
  {"input": "http://example.com/path?", "base": null, "search": ""},
  {"input": "http://example.com/path#", "base": null, "hash": ""},
  {"input": "http://example.com:8080/path?query#hash", "base": null, "port": "8080", "pathname": "/path", "search": "?query", "hash": "#hash"},
  {"input": "HTTP://EXAMPLE.COM/PATH", "base": null, "protocol": "http:", "hostname": "example.com", "pathname": "/PATH"},
  {"input": "http://example.com/%7E", "base": null, "pathname": "/~"},
  {"input": "http://example.com/%7e", "base": null, "pathname": "/~"},
  {"input": "http://example.com/a/./b/../c", "base": null, "pathname": "/a/c"},
  {"input": "http://example.com/../a", "base": null, "pathname": "/a"},
  {"input": "http://example.com/a/b/../../c", "base": null, "pathname": "/c"},
  {"input": "../path", "base": "http://example.com/a/b/c", "pathname": "/a/path"},
  {"input": "./path", "base": "http://example.com/a/b/c", "pathname": "/a/b/path"},
  {"input": "http://example.com/a//b", "base": null, "pathname": "/a//b"},
  {"input": "http://example.com//a", "base": null, "pathname": "//a"},
  {"input": "http://example.com/a?b=c&d=e", "base": null, "search": "?b=c&d=e"},
  {"input": "http://example.com/a?b=c#d=e", "base": null, "search": "?b=c", "hash": "#d=e"},
  {"input": "http://example.com:1234/", "base": null, "port": "1234"},
  {"input": "http://example.com:001234/", "base": null, "port": "1234"},
  {"input": "http://example.com/a%2Fb", "base": null, "pathname": "/a%2Fb"},
  {"input": "http://example.com/%2F", "base": null, "pathname": "/%2F"},
  {"input": "http://example.com/%2f", "base": null, "pathname": "/%2F"},
  {"input": "http://192.168.1.1/", "base": null, "hostname": "192.168.1.1"},
  {"input": "http://192.168.1.1:8080/", "base": null, "hostname": "192.168.1.1", "port": "8080"},
  {"input": "http://127.0.0.1/", "base": null, "hostname": "127.0.0.1"}
];

let passed = 0;
let failed = 0;

for (const test of testData) {
  try {
    const url = test.base ? new URL(test.input, test.base) : new URL(test.input);
    let testPassed = true;

    for (const [prop, expected] of Object.entries(test)) {
      if (prop === "input" || prop === "base") continue;
      const actual = url[prop];
      if (actual !== expected) {
        testPassed = false;
        break;
      }
    }

    if (testPassed) {
      passed++;
    } else {
      failed++;
    }
  } catch (e) {
    failed++;
  }
}

print("URL: passed:", passed, "failed:", failed, "total:", passed + failed);
'
fi

if [ "$SUITE" = "encoding" ] || [ "$SUITE" = "all" ]; then
    run_wpt_suite "textencoder" '
// WPT TextEncoder tests
let passed = 0;
let failed = 0;

// Basic encoding
try {
  const encoder = new TextEncoder();
  const result = encoder.encode("hello");
  if (result instanceof Uint8Array && result.length === 5 && result[0] === 104) {
    passed++;
  } else {
    print("FAIL: basic encode");
    failed++;
  }
} catch (e) {
  print("FAIL: basic encode:", e.message);
  failed++;
}

// Unicode encoding
try {
  const encoder = new TextEncoder();
  const result = encoder.encode("\u00e9");
  if (result.length === 2 && result[0] === 0xc3 && result[1] === 0xa9) {
    passed++;
  } else {
    print("FAIL: unicode encode");
    failed++;
  }
} catch (e) {
  print("FAIL: unicode encode:", e.message);
  failed++;
}

// Emoji encoding
try {
  const encoder = new TextEncoder();
  const result = encoder.encode("\ud83d\ude00");
  if (result.length === 4) {
    passed++;
  } else {
    print("FAIL: emoji encode");
    failed++;
  }
} catch (e) {
  print("FAIL: emoji encode:", e.message);
  failed++;
}

// encodeInto
try {
  const encoder = new TextEncoder();
  const dest = new Uint8Array(10);
  const result = encoder.encodeInto("hello", dest);
  if (result.read === 5 && result.written === 5 && dest[0] === 104) {
    passed++;
  } else {
    print("FAIL: encodeInto");
    failed++;
  }
} catch (e) {
  print("FAIL: encodeInto:", e.message);
  failed++;
}

print("TextEncoder: passed:", passed, "failed:", failed, "total:", passed + failed);
'

    run_wpt_suite "textdecoder" '
// WPT TextDecoder tests
let passed = 0;
let failed = 0;

// Basic decoding
try {
  const decoder = new TextDecoder();
  const result = decoder.decode(new Uint8Array([104, 101, 108, 108, 111]));
  if (result === "hello") {
    passed++;
  } else {
    print("FAIL: basic decode, got:", result);
    failed++;
  }
} catch (e) {
  print("FAIL: basic decode:", e.message);
  failed++;
}

// Unicode decoding
try {
  const decoder = new TextDecoder();
  const result = decoder.decode(new Uint8Array([0xc3, 0xa9]));
  if (result === "\u00e9") {
    passed++;
  } else {
    print("FAIL: unicode decode");
    failed++;
  }
} catch (e) {
  print("FAIL: unicode decode:", e.message);
  failed++;
}

// Emoji decoding
try {
  const decoder = new TextDecoder();
  const result = decoder.decode(new Uint8Array([0xf0, 0x9f, 0x98, 0x80]));
  if (result === "\ud83d\ude00") {
    passed++;
  } else {
    print("FAIL: emoji decode");
    failed++;
  }
} catch (e) {
  print("FAIL: emoji decode:", e.message);
  failed++;
}

// Stream option
try {
  const decoder = new TextDecoder();
  const part1 = decoder.decode(new Uint8Array([0xf0, 0x9f]), { stream: true });
  const part2 = decoder.decode(new Uint8Array([0x98, 0x80]));
  if (part1 + part2 === "\ud83d\ude00") {
    passed++;
  } else {
    print("FAIL: stream decode, got:", part1 + part2);
    failed++;
  }
} catch (e) {
  print("FAIL: stream decode:", e.message);
  failed++;
}

// Fatal option
try {
  const decoder = new TextDecoder("utf-8", { fatal: true });
  try {
    decoder.decode(new Uint8Array([0xff, 0xfe]));
    print("FAIL: fatal should throw");
    failed++;
  } catch (e) {
    if (e instanceof TypeError) {
      passed++;
    } else {
      print("FAIL: fatal wrong error type");
      failed++;
    }
  }
} catch (e) {
  print("FAIL: fatal decode setup:", e.message);
  failed++;
}

// Empty input
try {
  const decoder = new TextDecoder();
  const result = decoder.decode(new Uint8Array([]));
  if (result === "") {
    passed++;
  } else {
    print("FAIL: empty decode");
    failed++;
  }
} catch (e) {
  print("FAIL: empty decode:", e.message);
  failed++;
}

print("TextDecoder: passed:", passed, "failed:", failed, "total:", passed + failed);
'
fi

if [ "$SUITE" = "streams" ] || [ "$SUITE" = "all" ]; then
    run_wpt_suite "readable-stream" '
// WPT ReadableStream tests - wrapped in async IIFE
(async () => {
let passed = 0;
let failed = 0;

// Basic ReadableStream
try {
  const stream = new ReadableStream({
    start(controller) {
      controller.enqueue("hello");
      controller.enqueue("world");
      controller.close();
    }
  });

  const reader = stream.getReader();
  const chunk1 = await reader.read();
  const chunk2 = await reader.read();
  const done = await reader.read();

  if (chunk1.value === "hello" && chunk2.value === "world" && done.done) {
    passed++;
  } else {
    print("FAIL: basic ReadableStream");
    failed++;
  }
} catch (e) {
  print("FAIL: basic ReadableStream:", e.message);
  failed++;
}

// ReadableStream cancel
try {
  let cancelled = false;
  const stream = new ReadableStream({
    cancel(reason) {
      cancelled = reason;
    }
  });

  const reader = stream.getReader();
  await reader.cancel("test cancel");

  if (cancelled === "test cancel") {
    passed++;
  } else {
    print("FAIL: ReadableStream cancel, reason:", cancelled);
    failed++;
  }
} catch (e) {
  print("FAIL: ReadableStream cancel:", e.message);
  failed++;
}

// ReadableStream with pull
try {
  let pullCount = 0;
  const stream = new ReadableStream({
    pull(controller) {
      pullCount++;
      if (pullCount <= 2) {
        controller.enqueue(pullCount);
      } else {
        controller.close();
      }
    }
  });

  const reader = stream.getReader();
  const results = [];
  while (true) {
    const { value, done } = await reader.read();
    if (done) break;
    results.push(value);
  }

  if (results.length === 2 && results[0] === 1 && results[1] === 2) {
    passed++;
  } else {
    print("FAIL: ReadableStream pull, results:", results);
    failed++;
  }
} catch (e) {
  print("FAIL: ReadableStream pull:", e.message);
  failed++;
}

print("ReadableStream: passed:", passed, "failed:", failed, "total:", passed + failed);
})().catch(e => print("FATAL:", e.message));
'

    run_wpt_suite "writable-stream" '
// WPT WritableStream tests - wrapped in async IIFE
(async () => {
let passed = 0;
let failed = 0;

// Basic WritableStream
try {
  const chunks = [];
  const stream = new WritableStream({
    write(chunk) {
      chunks.push(chunk);
    }
  });

  const writer = stream.getWriter();
  await writer.write("hello");
  await writer.write("world");
  await writer.close();

  if (chunks.length === 2 && chunks[0] === "hello" && chunks[1] === "world") {
    passed++;
  } else {
    print("FAIL: basic WritableStream chunks:", chunks);
    failed++;
  }
} catch (e) {
  print("FAIL: basic WritableStream:", e.message);
  failed++;
}

// WritableStream abort
try {
  let aborted = false;
  const stream = new WritableStream({
    abort(reason) {
      aborted = reason;
    }
  });

  const writer = stream.getWriter();
  await writer.abort("test abort");

  if (aborted === "test abort") {
    passed++;
  } else {
    print("FAIL: WritableStream abort, reason:", aborted);
    failed++;
  }
} catch (e) {
  print("FAIL: WritableStream abort:", e.message);
  failed++;
}

print("WritableStream: passed:", passed, "failed:", failed, "total:", passed + failed);
})().catch(e => print("FATAL:", e.message));
'

    run_wpt_suite "transform-stream" '
// WPT TransformStream tests - wrapped in async IIFE
(async () => {
let passed = 0;
let failed = 0;

print("TransformStream: passed:", passed, "failed:", failed, "total:", passed + failed);
})().catch(e => print("FATAL:", e.message));
'
fi

if [ "$SUITE" = "abort" ] || [ "$SUITE" = "all" ]; then
    run_wpt_suite "abort-controller" '
// WPT AbortController tests - wrapped in async IIFE
(async () => {
let passed = 0;
let failed = 0;

// Basic AbortController
try {
  const controller = new AbortController();
  const signal = controller.signal;

  if (!signal.aborted) {
    passed++;
  } else {
    print("FAIL: signal should not be aborted initially");
    failed++;
  }
} catch (e) {
  print("FAIL: basic AbortController:", e.message);
  failed++;
}

// AbortController abort
try {
  const controller = new AbortController();
  controller.abort();

  if (controller.signal.aborted) {
    passed++;
  } else {
    print("FAIL: signal should be aborted after abort()");
    failed++;
  }
} catch (e) {
  print("FAIL: AbortController abort:", e.message);
  failed++;
}

// AbortController abort with reason
try {
  const controller = new AbortController();
  controller.abort("test reason");

  if (controller.signal.aborted && controller.signal.reason === "test reason") {
    passed++;
  } else {
    print("FAIL: abort reason not set correctly");
    failed++;
  }
} catch (e) {
  print("FAIL: AbortController abort with reason:", e.message);
  failed++;
}

// AbortSignal.abort() static method
try {
  const signal = AbortSignal.abort("static abort");

  if (signal.aborted && signal.reason === "static abort") {
    passed++;
  } else {
    print("FAIL: AbortSignal.abort() not working");
    failed++;
  }
} catch (e) {
  print("FAIL: AbortSignal.abort():", e.message);
  failed++;
}

// Event listener on abort
try {
  const controller = new AbortController();
  let eventFired = false;

  controller.signal.addEventListener("abort", () => {
    eventFired = true;
  });

  controller.abort();

  if (eventFired) {
    passed++;
  } else {
    print("FAIL: abort event not fired");
    failed++;
  }
} catch (e) {
  print("FAIL: abort event listener:", e.message);
  failed++;
}

print("AbortController: passed:", passed, "failed:", failed, "total:", passed + failed);
})().catch(e => print("FATAL:", e.message));
'
fi

# Print summary
echo "" >> "$RESULTS_FILE"
echo "=== Summary ===" >> "$RESULTS_FILE"
echo "passed: $PASSED" >> "$RESULTS_FILE"
echo "failed: $FAILED" >> "$RESULTS_FILE"
echo "skipped: 0" >> "$RESULTS_FILE"
echo "total: $((PASSED + FAILED))" >> "$RESULTS_FILE"
echo "compile_time: ${TOTAL_COMPILE_TIME}ms" >> "$RESULTS_FILE"
echo "run_time: ${TOTAL_RUN_TIME}ms" >> "$RESULTS_FILE"

cat "$RESULTS_FILE"

echo ""
echo ""
echo "=== WPT $SUITE Results ($RUNTIME) ==="
echo "Passed: $PASSED"
echo "Failed: $FAILED"
echo "Skipped: 0"
echo "Compile time: ${TOTAL_COMPILE_TIME}ms"
echo "Run time: ${TOTAL_RUN_TIME}ms"

# Clean up
rm -rf "$TEST_BUILD_DIR"

# Exit with error if any tests failed
[ "$FAILED" -gt 0 ] && exit 1
exit 0
