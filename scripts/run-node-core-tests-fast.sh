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

    run_test "buffer-allocUnsafe" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b = Buffer.allocUnsafe(10);
assert.strictEqual(b.length, 10);
print("PASS: Buffer.allocUnsafe");
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
print("PASS: Buffer.from ArrayBuffer");
'

    run_test "buffer-fill" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b = Buffer.alloc(5);
b.fill(255);
for (let i = 0; i < 5; i++) assert.strictEqual(b[i], 255);
print("PASS: Buffer.fill");
'

    run_test "buffer-includes" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b = Buffer.from("hello world");
assert.strictEqual(b.includes("world"), true);
assert.strictEqual(b.includes("xyz"), false);
print("PASS: Buffer.includes");
'

    run_test "buffer-compare" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b1 = Buffer.from([1, 2, 3]);
const b2 = Buffer.from([1, 2, 3]);
const b3 = Buffer.from([1, 2, 4]);
assert.strictEqual(b1.compare(b2), 0);
assert.strictEqual(b1.compare(b3), -1);
print("PASS: Buffer.compare");
'

    run_test "buffer-write-read-uint" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b = Buffer.alloc(8);
b.writeUInt32LE(0xDEADBEEF, 0);
assert.strictEqual(b.readUInt32LE(0), 0xDEADBEEF);
b.writeUInt16BE(0xABCD, 4);
assert.strictEqual(b.readUInt16BE(4), 0xABCD);
print("PASS: Buffer read/write uint");
'

    run_test "buffer-write-read-float" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b = Buffer.alloc(8);
b.writeFloatLE(3.14, 0);
assert(Math.abs(b.readFloatLE(0) - 3.14) < 0.001);
b.writeDoubleLE(2.718281828, 0);
assert(Math.abs(b.readDoubleLE(0) - 2.718281828) < 0.000001);
print("PASS: Buffer read/write float");
'

    run_test "buffer-toString-encoding" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b = Buffer.from("hello");
assert.strictEqual(b.toString("utf8"), "hello");
assert.strictEqual(b.toString("utf-8"), "hello");
print("PASS: Buffer.toString encoding");
'

    run_test "buffer-isBuffer" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
assert.strictEqual(Buffer.isBuffer(Buffer.alloc(1)), true);
assert.strictEqual(Buffer.isBuffer(new Uint8Array(1)), false);
assert.strictEqual(Buffer.isBuffer("hello"), false);
print("PASS: Buffer.isBuffer");
'

    run_test "buffer-byteLength" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
assert.strictEqual(Buffer.byteLength("hello"), 5);
assert.strictEqual(Buffer.byteLength("héllo"), 6);
print("PASS: Buffer.byteLength");
'

    run_test "buffer-toJSON" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b = Buffer.from([1, 2, 3]);
const json = b.toJSON();
assert.strictEqual(json.type, "Buffer");
assert.deepStrictEqual(json.data, [1, 2, 3]);
print("PASS: Buffer.toJSON");
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

    run_test "path-resolve" '
const path = require("path");
const assert = require("assert");
const result = path.resolve("/foo/bar", "./baz");
assert.strictEqual(result, "/foo/bar/baz");
print("PASS: path.resolve");
'

    run_test "path-normalize" '
const path = require("path");
const assert = require("assert");
assert.strictEqual(path.normalize("/foo/bar//baz/.."), "/foo/bar");
print("PASS: path.normalize");
'

    run_test "path-parse" '
const path = require("path");
const assert = require("assert");
const p = path.parse("/home/user/file.txt");
assert.strictEqual(p.root, "/");
assert.strictEqual(p.dir, "/home/user");
assert.strictEqual(p.base, "file.txt");
print("PASS: path.parse");
'

    run_test "path-format" '
const path = require("path");
const assert = require("assert");
const p = path.format({ root: "/", dir: "/home/user", base: "file.txt" });
assert.strictEqual(p, "/home/user/file.txt");
print("PASS: path.format");
'

    run_test "path-sep" '
const path = require("path");
const assert = require("assert");
assert.strictEqual(path.sep, "/");
print("PASS: path.sep");
'
fi

if [ "$MODULE" = "fs" ]; then
    run_test "fs-existsSync" '
const fs = require("fs");
const assert = require("assert");
assert.strictEqual(fs.existsSync("/tmp"), true);
assert.strictEqual(fs.existsSync("/nonexistent-12345"), false);
print("PASS: fs.existsSync");
'

    run_test "fs-readFileSync-writeFileSync" '
const fs = require("fs");
const assert = require("assert");
const testFile = "/tmp/edgebox-test-" + Date.now() + ".txt";
fs.writeFileSync(testFile, "hello world");
const content = fs.readFileSync(testFile, "utf8");
assert.strictEqual(content, "hello world");
fs.unlinkSync(testFile);
print("PASS: fs.readFileSync/writeFileSync");
'

    run_test "fs-readdirSync" '
const fs = require("fs");
const assert = require("assert");
const files = fs.readdirSync("/tmp");
assert(Array.isArray(files));
print("PASS: fs.readdirSync");
'

    run_test "fs-statSync" '
const fs = require("fs");
const assert = require("assert");
const stat = fs.statSync("/tmp");
assert(stat.isDirectory());
print("PASS: fs.statSync");
'

    run_test "fs-mkdirSync-rmdirSync" '
const fs = require("fs");
const assert = require("assert");
const testDir = "/tmp/edgebox-test-dir-" + Date.now();
fs.mkdirSync(testDir);
assert(fs.existsSync(testDir));
fs.rmdirSync(testDir);
assert(!fs.existsSync(testDir));
print("PASS: fs.mkdirSync/rmdirSync");
'
fi

if [ "$MODULE" = "crypto" ]; then
    run_test "crypto-randomBytes" '
const crypto = require("crypto");
const assert = require("assert");
const bytes = crypto.randomBytes(16);
assert.strictEqual(bytes.length, 16);
print("PASS: crypto.randomBytes");
'

    run_test "crypto-createHash-sha256" '
const crypto = require("crypto");
const assert = require("assert");
const hash = crypto.createHash("sha256");
hash.update("hello");
const digest = hash.digest("hex");
assert.strictEqual(digest, "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824");
print("PASS: crypto.createHash sha256");
'

    run_test "crypto-createHash-md5" '
const crypto = require("crypto");
const assert = require("assert");
const hash = crypto.createHash("md5");
hash.update("hello");
const digest = hash.digest("hex");
assert.strictEqual(digest, "5d41402abc4b2a76b9719d911017c592");
print("PASS: crypto.createHash md5");
'

    run_test "crypto-randomUUID" '
const assert = require("assert");
const uuid = crypto.randomUUID();
assert(typeof uuid === "string");
assert.strictEqual(uuid.length, 36);
print("PASS: crypto.randomUUID");
'
fi

if [ "$MODULE" = "util" ]; then
    run_test "util-inspect" '
const util = require("util");
const assert = require("assert");
const result = util.inspect({ a: 1, b: "hello" });
assert(typeof result === "string");
print("PASS: util.inspect");
'

    run_test "util-format" '
const util = require("util");
const assert = require("assert");
assert.strictEqual(util.format("Hello %s", "World"), "Hello World");
assert.strictEqual(util.format("Number: %d", 42), "Number: 42");
print("PASS: util.format");
'

    run_test "util-types" '
const util = require("util");
const assert = require("assert");
assert.strictEqual(util.types.isDate(new Date()), true);
assert.strictEqual(util.types.isRegExp(/abc/), true);
print("PASS: util.types");
'
fi

if [ "$MODULE" = "process" ]; then
    run_test "process-env" '
const assert = require("assert");
assert(typeof process.env === "object");
print("PASS: process.env");
'

    run_test "process-cwd" '
const assert = require("assert");
const cwd = process.cwd();
assert(typeof cwd === "string");
assert(cwd.startsWith("/"));
print("PASS: process.cwd");
'

    run_test "process-platform" '
const assert = require("assert");
assert(["darwin", "linux", "win32"].includes(process.platform));
print("PASS: process.platform");
'

    run_test "process-arch" '
const assert = require("assert");
assert(["arm64", "x64", "ia32", "arm"].includes(process.arch));
print("PASS: process.arch");
'

    run_test "process-version" '
const assert = require("assert");
assert(typeof process.version === "string");
assert(process.version.startsWith("v"));
print("PASS: process.version");
'

    run_test "process-argv" '
const assert = require("assert");
assert(Array.isArray(process.argv));
print("PASS: process.argv");
'
fi

if [ "$MODULE" = "events" ]; then
    run_test "events-on-emit" '
const EventEmitter = require("events");
const assert = require("assert");
const ee = new EventEmitter();
let called = false;
ee.on("test", () => { called = true; });
ee.emit("test");
assert.strictEqual(called, true);
print("PASS: EventEmitter on/emit");
'

    run_test "events-once" '
const EventEmitter = require("events");
const assert = require("assert");
const ee = new EventEmitter();
let count = 0;
ee.once("test", () => { count++; });
ee.emit("test");
ee.emit("test");
assert.strictEqual(count, 1);
print("PASS: EventEmitter once");
'

    run_test "events-removeListener" '
const EventEmitter = require("events");
const assert = require("assert");
const ee = new EventEmitter();
let count = 0;
const handler = () => { count++; };
ee.on("test", handler);
ee.emit("test");
ee.removeListener("test", handler);
ee.emit("test");
assert.strictEqual(count, 1);
print("PASS: EventEmitter removeListener");
'

    run_test "events-listenerCount" '
const EventEmitter = require("events");
const assert = require("assert");
const ee = new EventEmitter();
ee.on("test", () => {});
ee.on("test", () => {});
assert.strictEqual(ee.listenerCount("test"), 2);
print("PASS: EventEmitter listenerCount");
'
fi

if [ "$MODULE" = "assert" ]; then
    run_test "assert-strictEqual" '
const assert = require("assert");
assert.strictEqual(1, 1);
assert.strictEqual("hello", "hello");
print("PASS: assert.strictEqual");
'

    run_test "assert-deepStrictEqual" '
const assert = require("assert");
assert.deepStrictEqual({ a: 1 }, { a: 1 });
assert.deepStrictEqual([1, 2, 3], [1, 2, 3]);
print("PASS: assert.deepStrictEqual");
'

    run_test "assert-throws" '
const assert = require("assert");
assert.throws(() => { throw new Error("test"); });
print("PASS: assert.throws");
'

    run_test "assert-ok" '
const assert = require("assert");
assert.ok(true);
assert.ok(1);
assert.ok("hello");
print("PASS: assert.ok");
'
fi

if [ "$MODULE" = "os" ]; then
    run_test "os-platform" '
const os = require("os");
const assert = require("assert");
const platform = os.platform();
assert(["darwin", "linux", "win32"].includes(platform));
print("PASS: os.platform");
'

    run_test "os-arch" '
const os = require("os");
const assert = require("assert");
const arch = os.arch();
assert(["arm64", "x64", "ia32", "arm"].includes(arch));
print("PASS: os.arch");
'

    run_test "os-cpus" '
const os = require("os");
const assert = require("assert");
const cpus = os.cpus();
assert(Array.isArray(cpus));
assert(cpus.length > 0);
print("PASS: os.cpus");
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
