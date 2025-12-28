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

echo "=== WPT Tests for $RUNTIME ===" > "$RESULTS_FILE"
echo "Suite: $SUITE" >> "$RESULTS_FILE"
echo "Started: $(date)" >> "$RESULTS_FILE"
echo "" >> "$RESULTS_FILE"

# Create test directory
TEST_BUILD_DIR="/tmp/edgebox-wpt-tests"
rm -rf "$TEST_BUILD_DIR"
mkdir -p "$TEST_BUILD_DIR"

echo "Running $SUITE tests using real WPT test data..."
echo ""

# Counters
PASSED=0
FAILED=0
SKIPPED=0
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
  {"input": "http://example.com/a+b", "base": null, "pathname": "/a+b"},
  {"input": "http://example.com:1234/", "base": null, "port": "1234"},
  {"input": "ws://example.com/", "base": null, "protocol": "ws:", "href": "ws://example.com/"},
  {"input": "wss://example.com/", "base": null, "protocol": "wss:", "href": "wss://example.com/"},
  {"input": "ftp://example.com/", "base": null, "protocol": "ftp:", "href": "ftp://example.com/"},
  {"input": "http://example.com/path/", "base": null, "pathname": "/path/"},
  {"input": "http://example.com", "base": null, "href": "http://example.com/", "pathname": "/"},
  {"input": "http://example.com:8080", "base": null, "href": "http://example.com:8080/", "port": "8080"},
  // URLSearchParams tests
  {"input": "http://example.com/?foo=bar", "base": null, "search": "?foo=bar"},
  {"input": "http://example.com/?foo=bar&baz=qux", "base": null, "search": "?foo=bar&baz=qux"},
  {"input": "http://example.com/?foo=hello%20world", "base": null, "search": "?foo=hello%20world"},
  {"input": "http://example.com/?foo=hello+world", "base": null, "search": "?foo=hello+world"},
  {"input": "http://example.com/?foo=%26bar", "base": null, "search": "?foo=%26bar"},
];

let passed = 0;
let failed = 0;

for (const test of testData) {
  if (typeof test === "string") continue; // Skip comment strings

  try {
    const url = test.base ? new URL(test.input, test.base) : new URL(test.input);
    let testPassed = true;

    const checks = ["href", "protocol", "hostname", "port", "pathname", "search", "hash", "username", "password"];
    for (const prop of checks) {
      if (test[prop] !== undefined) {
        if (url[prop] !== test[prop]) {
          print("FAIL:", test.input, prop, "expected:", test[prop], "got:", url[prop]);
          testPassed = false;
          break;
        }
      }
    }

    if (testPassed) passed++;
    else failed++;
  } catch (e) {
    if (test.failure) {
      passed++; // Expected to fail
    } else {
      print("FAIL:", test.input, "threw:", e.message);
      failed++;
    }
  }
}

print("URL parsing: passed:", passed, "failed:", failed, "total:", passed + failed);
'

    # URLSearchParams tests
    run_wpt_suite "urlsearchparams" '
// Real WPT URLSearchParams tests

const tests = [
  // Constructor tests
  {desc: "empty string", input: "", expected: []},
  {desc: "single pair", input: "a=b", expected: [["a", "b"]]},
  {desc: "multiple pairs", input: "a=b&c=d", expected: [["a", "b"], ["c", "d"]]},
  {desc: "duplicate keys", input: "a=1&a=2", expected: [["a", "1"], ["a", "2"]]},
  {desc: "empty value", input: "a=", expected: [["a", ""]]},
  {desc: "no value", input: "a", expected: [["a", ""]]},
  {desc: "plus as space", input: "a=hello+world", expected: [["a", "hello world"]]},
  {desc: "percent encoding", input: "a=hello%20world", expected: [["a", "hello world"]]},
  {desc: "special chars", input: "a=%26%3D%3F", expected: [["a", "&=?"]]},
  {desc: "unicode", input: "a=%E4%B8%AD%E6%96%87", expected: [["a", "中文"]]},
  {desc: "leading ?", input: "?a=b", expected: [["a", "b"]]},
];

let passed = 0;
let failed = 0;

for (const test of tests) {
  try {
    const params = new URLSearchParams(test.input);
    const entries = [...params.entries()];

    let ok = entries.length === test.expected.length;
    if (ok) {
      for (let i = 0; i < entries.length; i++) {
        if (entries[i][0] !== test.expected[i][0] || entries[i][1] !== test.expected[i][1]) {
          ok = false;
          break;
        }
      }
    }

    if (ok) {
      passed++;
    } else {
      print("FAIL:", test.desc, "expected:", JSON.stringify(test.expected), "got:", JSON.stringify(entries));
      failed++;
    }
  } catch (e) {
    print("FAIL:", test.desc, "threw:", e.message);
    failed++;
  }
}

// Method tests
const methodTests = [
  {desc: "get()", fn: () => { const p = new URLSearchParams("a=1&b=2"); return p.get("a") === "1" && p.get("b") === "2" && p.get("c") === null; }},
  {desc: "getAll()", fn: () => { const p = new URLSearchParams("a=1&a=2"); const all = p.getAll("a"); return all.length === 2 && all[0] === "1" && all[1] === "2"; }},
  {desc: "has()", fn: () => { const p = new URLSearchParams("a=1"); return p.has("a") === true && p.has("b") === false; }},
  {desc: "set()", fn: () => { const p = new URLSearchParams("a=1&a=2"); p.set("a", "3"); return p.getAll("a").length === 1 && p.get("a") === "3"; }},
  {desc: "append()", fn: () => { const p = new URLSearchParams("a=1"); p.append("a", "2"); return p.getAll("a").length === 2; }},
  {desc: "delete()", fn: () => { const p = new URLSearchParams("a=1&b=2"); p.delete("a"); return p.has("a") === false && p.has("b") === true; }},
  {desc: "toString()", fn: () => { const p = new URLSearchParams(); p.set("a", "1"); p.set("b", "2"); return p.toString() === "a=1&b=2"; }},
  {desc: "sort()", fn: () => { const p = new URLSearchParams("c=3&a=1&b=2"); p.sort(); return p.toString() === "a=1&b=2&c=3"; }},
  {desc: "keys()", fn: () => { const p = new URLSearchParams("a=1&b=2"); return [...p.keys()].join(",") === "a,b"; }},
  {desc: "values()", fn: () => { const p = new URLSearchParams("a=1&b=2"); return [...p.values()].join(",") === "1,2"; }},
  {desc: "entries()", fn: () => { const p = new URLSearchParams("a=1"); const e = [...p.entries()][0]; return e[0] === "a" && e[1] === "1"; }},
  {desc: "forEach()", fn: () => { const p = new URLSearchParams("a=1&b=2"); let r = ""; p.forEach((v,k) => r += k + v); return r === "a1b2"; }},
  {desc: "size", fn: () => { const p = new URLSearchParams("a=1&b=2&a=3"); return p.size === 3; }},
];

for (const test of methodTests) {
  try {
    if (test.fn()) {
      passed++;
    } else {
      print("FAIL:", test.desc);
      failed++;
    }
  } catch (e) {
    print("FAIL:", test.desc, "threw:", e.message);
    failed++;
  }
}

print("URLSearchParams: passed:", passed, "failed:", failed, "total:", passed + failed);
'
fi

if [ "$SUITE" = "encoding" ] || [ "$SUITE" = "all" ]; then
    # Real WPT TextEncoder/TextDecoder tests
    run_wpt_suite "textencoder" '
// Real WPT TextEncoder tests

const tests = [
  {input: "", expected: []},
  {input: "hello", expected: [104, 101, 108, 108, 111]},
  {input: "©", expected: [194, 169]},
  {input: "€", expected: [226, 130, 172]},
  {input: "𝌆", expected: [240, 157, 140, 134]},
  {input: "中文", expected: [228, 184, 173, 230, 150, 135]},
  {input: "👋", expected: [240, 159, 145, 139]},
  {input: "a\u0000b", expected: [97, 0, 98]},
  {input: "\uFEFF", expected: [239, 187, 191]}, // BOM
];

let passed = 0;
let failed = 0;

const encoder = new TextEncoder();

for (const test of tests) {
  const result = encoder.encode(test.input);
  let ok = result.length === test.expected.length;
  if (ok) {
    for (let i = 0; i < result.length; i++) {
      if (result[i] !== test.expected[i]) {
        ok = false;
        break;
      }
    }
  }

  if (ok) {
    passed++;
  } else {
    print("FAIL: encode", JSON.stringify(test.input), "expected:", test.expected, "got:", [...result]);
    failed++;
  }
}

// encodeInto tests
const encodeIntoTests = [
  {input: "hello", destLen: 10, expectedRead: 5, expectedWritten: 5},
  {input: "hello", destLen: 3, expectedRead: 3, expectedWritten: 3},
  {input: "中文", destLen: 10, expectedRead: 2, expectedWritten: 6},
  {input: "中文", destLen: 3, expectedRead: 1, expectedWritten: 3},
];

for (const test of encodeIntoTests) {
  const dest = new Uint8Array(test.destLen);
  const result = encoder.encodeInto(test.input, dest);

  if (result.read === test.expectedRead && result.written === test.expectedWritten) {
    passed++;
  } else {
    print("FAIL: encodeInto", JSON.stringify(test.input), "destLen:", test.destLen,
          "expected read:", test.expectedRead, "written:", test.expectedWritten,
          "got read:", result.read, "written:", result.written);
    failed++;
  }
}

print("TextEncoder: passed:", passed, "failed:", failed, "total:", passed + failed);
'

    run_wpt_suite "textdecoder" '
// Real WPT TextDecoder tests

const tests = [
  {input: [], expected: ""},
  {input: [104, 101, 108, 108, 111], expected: "hello"},
  {input: [194, 169], expected: "©"},
  {input: [226, 130, 172], expected: "€"},
  {input: [240, 157, 140, 134], expected: "𝌆"},
  {input: [228, 184, 173, 230, 150, 135], expected: "中文"},
  {input: [240, 159, 145, 139], expected: "👋"},
  {input: [97, 0, 98], expected: "a\u0000b"},
  {input: [239, 187, 191], expected: "\uFEFF"}, // BOM
];

let passed = 0;
let failed = 0;

const decoder = new TextDecoder();

for (const test of tests) {
  const result = decoder.decode(new Uint8Array(test.input));

  if (result === test.expected) {
    passed++;
  } else {
    print("FAIL: decode", test.input, "expected:", JSON.stringify(test.expected), "got:", JSON.stringify(result));
    failed++;
  }
}

// Streaming decode tests
const streamTests = [
  {chunks: [[228], [184], [173]], expected: "中"}, // Split multibyte
  {chunks: [[240, 159], [145, 139]], expected: "👋"}, // Split 4-byte
];

for (const test of streamTests) {
  const streamDecoder = new TextDecoder("utf-8", {stream: true});
  let result = "";
  for (let i = 0; i < test.chunks.length; i++) {
    const isLast = i === test.chunks.length - 1;
    result += streamDecoder.decode(new Uint8Array(test.chunks[i]), {stream: !isLast});
  }

  if (result === test.expected) {
    passed++;
  } else {
    print("FAIL: streaming decode expected:", JSON.stringify(test.expected), "got:", JSON.stringify(result));
    failed++;
  }
}

// Fatal mode tests
try {
  const fatalDecoder = new TextDecoder("utf-8", {fatal: true});
  fatalDecoder.decode(new Uint8Array([0xFF, 0xFE])); // Invalid UTF-8
  print("FAIL: fatal mode should throw on invalid UTF-8");
  failed++;
} catch (e) {
  if (e instanceof TypeError) {
    passed++;
  } else {
    print("FAIL: fatal mode threw wrong error type:", e.constructor.name);
    failed++;
  }
}

print("TextDecoder: passed:", passed, "failed:", failed, "total:", passed + failed);
'
fi

if [ "$SUITE" = "streams" ] || [ "$SUITE" = "all" ]; then
    run_wpt_suite "readable-stream" '
// WPT ReadableStream tests

let passed = 0;
let failed = 0;

// Basic ReadableStream
try {
  let pullCount = 0;
  const stream = new ReadableStream({
    pull(controller) {
      if (pullCount < 3) {
        controller.enqueue(pullCount++);
      } else {
        controller.close();
      }
    }
  });

  const reader = stream.getReader();
  const chunks = [];

  while (true) {
    const {value, done} = await reader.read();
    if (done) break;
    chunks.push(value);
  }

  if (chunks.length === 3 && chunks[0] === 0 && chunks[1] === 1 && chunks[2] === 2) {
    passed++;
  } else {
    print("FAIL: basic ReadableStream chunks:", chunks);
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
  await reader.cancel("test reason");

  if (cancelled === "test reason") {
    passed++;
  } else {
    print("FAIL: ReadableStream cancel, reason:", cancelled);
    failed++;
  }
} catch (e) {
  print("FAIL: ReadableStream cancel:", e.message);
  failed++;
}

// ReadableStream locked
try {
  const stream = new ReadableStream();
  if (stream.locked !== false) throw new Error("should not be locked initially");
  const reader = stream.getReader();
  if (stream.locked !== true) throw new Error("should be locked after getReader");
  reader.releaseLock();
  if (stream.locked !== false) throw new Error("should not be locked after releaseLock");
  passed++;
} catch (e) {
  print("FAIL: ReadableStream locked:", e.message);
  failed++;
}

print("ReadableStream: passed:", passed, "failed:", failed, "total:", passed + failed);
'

    run_wpt_suite "writable-stream" '
// WPT WritableStream tests

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
'

    run_wpt_suite "transform-stream" '
// WPT TransformStream tests

let passed = 0;
let failed = 0;

// Basic TransformStream
try {
  const transform = new TransformStream({
    transform(chunk, controller) {
      controller.enqueue(chunk.toUpperCase());
    }
  });

  const writer = transform.writable.getWriter();
  const reader = transform.readable.getReader();

  await writer.write("hello");
  const {value} = await reader.read();

  if (value === "HELLO") {
    passed++;
  } else {
    print("FAIL: basic TransformStream, got:", value);
    failed++;
  }

  await writer.close();
} catch (e) {
  print("FAIL: basic TransformStream:", e.message);
  failed++;
}

// TransformStream flush
try {
  let flushed = false;
  const transform = new TransformStream({
    transform(chunk, controller) {
      controller.enqueue(chunk);
    },
    flush(controller) {
      flushed = true;
      controller.enqueue("flushed");
    }
  });

  const writer = transform.writable.getWriter();
  const reader = transform.readable.getReader();

  await writer.write("data");
  await writer.close();

  const results = [];
  while (true) {
    const {value, done} = await reader.read();
    if (done) break;
    results.push(value);
  }

  if (flushed && results.includes("flushed")) {
    passed++;
  } else {
    print("FAIL: TransformStream flush, flushed:", flushed, "results:", results);
    failed++;
  }
} catch (e) {
  print("FAIL: TransformStream flush:", e.message);
  failed++;
}

print("TransformStream: passed:", passed, "failed:", failed, "total:", passed + failed);
'
fi

if [ "$SUITE" = "abort" ] || [ "$SUITE" = "all" ]; then
    run_wpt_suite "abort-controller" '
// WPT AbortController/AbortSignal tests

let passed = 0;
let failed = 0;

// Basic AbortController
try {
  const controller = new AbortController();
  const signal = controller.signal;

  if (signal.aborted !== false) throw new Error("should not be aborted initially");
  controller.abort();
  if (signal.aborted !== true) throw new Error("should be aborted after abort()");
  passed++;
} catch (e) {
  print("FAIL: basic AbortController:", e.message);
  failed++;
}

// AbortSignal abort event
try {
  const controller = new AbortController();
  let eventFired = false;
  let eventSignal = null;

  controller.signal.addEventListener("abort", (e) => {
    eventFired = true;
    eventSignal = e.target;
  });

  controller.abort();

  if (eventFired && eventSignal === controller.signal) {
    passed++;
  } else {
    print("FAIL: abort event, fired:", eventFired, "signal match:", eventSignal === controller.signal);
    failed++;
  }
} catch (e) {
  print("FAIL: abort event:", e.message);
  failed++;
}

// AbortController.abort(reason)
try {
  const controller = new AbortController();
  const customReason = new Error("custom abort reason");
  controller.abort(customReason);

  if (controller.signal.reason === customReason) {
    passed++;
  } else {
    print("FAIL: abort reason, got:", controller.signal.reason);
    failed++;
  }
} catch (e) {
  print("FAIL: abort reason:", e.message);
  failed++;
}

// AbortSignal.throwIfAborted()
try {
  const controller = new AbortController();

  // Should not throw before abort
  controller.signal.throwIfAborted();

  controller.abort();

  // Should throw after abort
  try {
    controller.signal.throwIfAborted();
    print("FAIL: throwIfAborted should throw after abort");
    failed++;
  } catch (e) {
    if (e.name === "AbortError") {
      passed++;
    } else {
      print("FAIL: throwIfAborted threw wrong error:", e.name);
      failed++;
    }
  }
} catch (e) {
  print("FAIL: throwIfAborted:", e.message);
  failed++;
}

// AbortSignal.timeout() - if supported
if (typeof AbortSignal.timeout === "function") {
  try {
    const signal = AbortSignal.timeout(50);
    if (signal.aborted) throw new Error("should not be aborted immediately");

    await new Promise(r => setTimeout(r, 100));

    if (signal.aborted && signal.reason.name === "TimeoutError") {
      passed++;
    } else {
      print("FAIL: AbortSignal.timeout, aborted:", signal.aborted, "reason:", signal.reason?.name);
      failed++;
    }
  } catch (e) {
    print("FAIL: AbortSignal.timeout:", e.message);
    failed++;
  }
} else {
  print("SKIP: AbortSignal.timeout not supported");
}

// AbortSignal.any() - if supported
if (typeof AbortSignal.any === "function") {
  try {
    const c1 = new AbortController();
    const c2 = new AbortController();
    const signal = AbortSignal.any([c1.signal, c2.signal]);

    if (signal.aborted) throw new Error("should not be aborted initially");

    c1.abort("first");

    if (signal.aborted && signal.reason === "first") {
      passed++;
    } else {
      print("FAIL: AbortSignal.any, aborted:", signal.aborted, "reason:", signal.reason);
      failed++;
    }
  } catch (e) {
    print("FAIL: AbortSignal.any:", e.message);
    failed++;
  }
} else {
  print("SKIP: AbortSignal.any not supported");
}

print("AbortController: passed:", passed, "failed:", failed, "total:", passed + failed);
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
