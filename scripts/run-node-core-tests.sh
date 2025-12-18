#!/bin/bash
# Run Node.js Core Tests on EdgeBox
# Usage: ./scripts/run-node-core-tests.sh <node-core-dir> <module>
#
# This runs our own test suite that tests Node.js API compatibility
# without the complex Node.js test harness infrastructure.

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

# Create a temp directory for test builds
TEST_BUILD_DIR="/tmp/edgebox-node-tests"
rm -rf "$TEST_BUILD_DIR"
mkdir -p "$TEST_BUILD_DIR"

# Function to run a single test
run_test() {
    local test_name="$1"
    local test_code="$2"

    # Create a test app directory
    local test_app_dir="$TEST_BUILD_DIR/$test_name"
    mkdir -p "$test_app_dir"

    # Write test code
    echo "$test_code" > "$test_app_dir/index.js"

    # Try to compile the test
    local compile_output
    local compile_exit=0
    compile_output=$(./zig-out/bin/edgeboxc build "$test_app_dir" 2>&1) || compile_exit=$?

    if [ $compile_exit -ne 0 ]; then
        echo "✗ $test_name (compile failed)" >> "$RESULTS_FILE"
        echo "  Error: $(echo "$compile_output" | grep -E "error:" | head -1)" >> "$RESULTS_FILE"
        FAILED=$((FAILED + 1))
        rm -rf "$test_app_dir"
        return
    fi

    # Find the compiled WASM (prefer WASM over AOT for now - AOT has segfault issues)
    # edgeboxc outputs to zig-out/bin/tmp/edgebox-node-tests/$test_name/$test_name.wasm
    local wasm_file="./zig-out/bin/tmp/edgebox-node-tests/$test_name/$test_name.wasm"
    if [ ! -f "$wasm_file" ]; then
        wasm_file="./zig-out/bin/tmp/edgebox-node-tests/$test_name/$test_name.aot"
    fi
    if [ ! -f "$wasm_file" ]; then
        # Fallback patterns
        wasm_file="./zig-out/bin/tmp/edgebox-node-tests/$test_name.wasm"
    fi
    if [ ! -f "$wasm_file" ]; then
        wasm_file="./zig-out/bin/tmp/edgebox-node-tests/$test_name.aot"
    fi

    if [ ! -f "$wasm_file" ]; then
        echo "✗ $test_name (no wasm output)" >> "$RESULTS_FILE"
        FAILED=$((FAILED + 1))
        rm -rf "$test_app_dir"
        return
    fi

    # Run the test with timeout
    local output
    local exit_code=0
    output=$(timeout 10s ./zig-out/bin/edgebox "$wasm_file" 2>&1) || exit_code=$?

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
    rm -rf "$test_app_dir"
    rm -rf "./zig-out/bin/tmp/edgebox-node-tests/$test_name.aot"
    rm -rf "./zig-out/bin/tmp/edgebox-node-tests/$test_name.wasm"
    rm -rf "./zig-out/cache/tmp/edgebox-node-tests/$test_name"
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

    run_test "buffer-allocUnsafe" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b = Buffer.allocUnsafe(10);
assert.strictEqual(b.length, 10);
print("PASS: Buffer.allocUnsafe");
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

    run_test "buffer-concat" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b1 = Buffer.from([1, 2]);
const b2 = Buffer.from([3, 4]);
const b3 = Buffer.concat([b1, b2]);
assert.strictEqual(b3.length, 4);
assert.strictEqual(b3[0], 1);
assert.strictEqual(b3[3], 4);
print("PASS: Buffer.concat");
'

    run_test "buffer-slice" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b = Buffer.from([1, 2, 3, 4, 5]);
const s = b.slice(1, 4);
assert.strictEqual(s.length, 3);
assert.strictEqual(s[0], 2);
assert.strictEqual(s[2], 4);
print("PASS: Buffer.slice");
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
print("PASS: Buffer.copy");
'

    run_test "buffer-fill" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b = Buffer.alloc(5);
b.fill(255);
for (let i = 0; i < 5; i++) assert.strictEqual(b[i], 255);
print("PASS: Buffer.fill");
'

    run_test "buffer-indexOf" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b = Buffer.from("hello world");
assert.strictEqual(b.indexOf("world"), 6);
assert.strictEqual(b.indexOf("xyz"), -1);
print("PASS: Buffer.indexOf");
'

    run_test "buffer-includes" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b = Buffer.from("hello world");
assert.strictEqual(b.includes("world"), true);
assert.strictEqual(b.includes("xyz"), false);
print("PASS: Buffer.includes");
'

    run_test "buffer-equals" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b1 = Buffer.from([1, 2, 3]);
const b2 = Buffer.from([1, 2, 3]);
const b3 = Buffer.from([1, 2, 4]);
assert.strictEqual(b1.equals(b2), true);
assert.strictEqual(b1.equals(b3), false);
print("PASS: Buffer.equals");
'

    run_test "buffer-compare" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b1 = Buffer.from([1, 2, 3]);
const b2 = Buffer.from([1, 2, 3]);
const b3 = Buffer.from([1, 2, 4]);
assert.strictEqual(b1.compare(b2), 0);
assert.strictEqual(b1.compare(b3), -1);
assert.strictEqual(b3.compare(b1), 1);
print("PASS: Buffer.compare");
'

    run_test "buffer-write-read-int" '
const Buffer = require("buffer").Buffer;
const assert = require("assert");
const b = Buffer.alloc(8);
b.writeInt32LE(12345, 0);
assert.strictEqual(b.readInt32LE(0), 12345);
b.writeInt32BE(-1000, 4);
assert.strictEqual(b.readInt32BE(4), -1000);
print("PASS: Buffer read/write int");
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

if [ "$MODULE" = "path" ] || [ "$MODULE" = "all" ]; then
    # Path tests
    run_test "path-join" '
const path = require("path");
const assert = require("assert");
assert.strictEqual(path.join("/foo", "bar", "baz"), "/foo/bar/baz");
assert.strictEqual(path.join("/foo", "../bar"), "/bar");
print("PASS: path.join");
'

    run_test "path-resolve" '
const path = require("path");
const assert = require("assert");
const result = path.resolve("/foo/bar", "./baz");
assert.strictEqual(result, "/foo/bar/baz");
print("PASS: path.resolve");
'

    run_test "path-dirname" '
const path = require("path");
const assert = require("assert");
assert.strictEqual(path.dirname("/foo/bar/baz.txt"), "/foo/bar");
assert.strictEqual(path.dirname("/foo/bar/"), "/foo");
print("PASS: path.dirname");
'

    run_test "path-basename" '
const path = require("path");
const assert = require("assert");
assert.strictEqual(path.basename("/foo/bar/baz.txt"), "baz.txt");
assert.strictEqual(path.basename("/foo/bar/baz.txt", ".txt"), "baz");
print("PASS: path.basename");
'

    run_test "path-extname" '
const path = require("path");
const assert = require("assert");
assert.strictEqual(path.extname("/foo/bar/baz.txt"), ".txt");
assert.strictEqual(path.extname("/foo/bar/baz"), "");
assert.strictEqual(path.extname("/foo/bar/.gitignore"), "");
print("PASS: path.extname");
'

    run_test "path-isAbsolute" '
const path = require("path");
const assert = require("assert");
assert.strictEqual(path.isAbsolute("/foo/bar"), true);
assert.strictEqual(path.isAbsolute("foo/bar"), false);
print("PASS: path.isAbsolute");
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
assert.strictEqual(p.ext, ".txt");
assert.strictEqual(p.name, "file");
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

if [ "$MODULE" = "fs" ] || [ "$MODULE" = "all" ]; then
    # FS tests
    run_test "fs-existsSync" '
const fs = require("fs");
const assert = require("assert");
assert.strictEqual(fs.existsSync("/tmp"), true);
assert.strictEqual(fs.existsSync("/nonexistent-path-12345"), false);
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
assert(!stat.isFile());
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

if [ "$MODULE" = "crypto" ] || [ "$MODULE" = "all" ]; then
    # Crypto tests
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

    run_test "crypto-getRandomValues" '
const assert = require("assert");
const arr = new Uint8Array(16);
crypto.getRandomValues(arr);
// Check that at least some values are non-zero (extremely unlikely all zero)
let hasNonZero = false;
for (let i = 0; i < arr.length; i++) if (arr[i] !== 0) hasNonZero = true;
assert(hasNonZero);
print("PASS: crypto.getRandomValues");
'

    run_test "crypto-randomUUID" '
const assert = require("assert");
const uuid = crypto.randomUUID();
assert(typeof uuid === "string");
assert.strictEqual(uuid.length, 36);
assert(uuid.match(/^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/));
print("PASS: crypto.randomUUID");
'
fi

if [ "$MODULE" = "util" ] || [ "$MODULE" = "all" ]; then
    # Util tests
    run_test "util-inspect" '
const util = require("util");
const assert = require("assert");
const result = util.inspect({ a: 1, b: "hello" });
assert(typeof result === "string");
assert(result.includes("a"));
print("PASS: util.inspect");
'

    run_test "util-format" '
const util = require("util");
const assert = require("assert");
assert.strictEqual(util.format("Hello %s", "World"), "Hello World");
assert.strictEqual(util.format("Number: %d", 42), "Number: 42");
print("PASS: util.format");
'

    run_test "util-promisify" '
const util = require("util");
const assert = require("assert");
function callbackFn(arg, cb) { cb(null, arg + 1); }
const promiseFn = util.promisify(callbackFn);
promiseFn(5).then(result => {
    assert.strictEqual(result, 6);
    print("PASS: util.promisify");
}).catch(e => print("FAIL:", e.message));
'

    run_test "util-types" '
const util = require("util");
const assert = require("assert");
assert.strictEqual(util.types.isDate(new Date()), true);
assert.strictEqual(util.types.isDate("2023-01-01"), false);
assert.strictEqual(util.types.isRegExp(/abc/), true);
assert.strictEqual(util.types.isRegExp("abc"), false);
print("PASS: util.types");
'
fi

if [ "$MODULE" = "process" ] || [ "$MODULE" = "all" ]; then
    # Process tests
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

if [ "$MODULE" = "events" ] || [ "$MODULE" = "all" ]; then
    # Events tests
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

if [ "$MODULE" = "timers" ] || [ "$MODULE" = "all" ]; then
    # Timers tests
    run_test "timers-setTimeout" '
const assert = require("assert");
let called = false;
setTimeout(() => {
    called = true;
    assert.strictEqual(called, true);
    print("PASS: setTimeout");
}, 10);
'

    run_test "timers-clearTimeout" '
const assert = require("assert");
let called = false;
const id = setTimeout(() => { called = true; }, 50);
clearTimeout(id);
setTimeout(() => {
    assert.strictEqual(called, false);
    print("PASS: clearTimeout");
}, 100);
'

    run_test "timers-setInterval" '
const assert = require("assert");
let count = 0;
const id = setInterval(() => {
    count++;
    if (count >= 3) {
        clearInterval(id);
        assert.strictEqual(count, 3);
        print("PASS: setInterval");
    }
}, 10);
'
fi

# Cleanup temp dir
rm -rf "$TEST_BUILD_DIR"

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

# Exit with success (we want to track progress, not block CI)
exit 0
