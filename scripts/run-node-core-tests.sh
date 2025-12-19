#!/bin/bash
# Run Node.js Core Tests with Multiple Runtimes
# Usage: ./scripts/run-node-core-tests.sh <runtime> <module> [threads]
#
# Runtimes: edgebox, node, bun
# This runs our own test suite that tests Node.js API compatibility.

set -e

RUNTIME="${1:-edgebox}"
MODULE="${2:-all}"
THREADS="${3:-3}"  # Default 3 threads (GitHub Actions macos-latest has 3 cores)
RESULTS_FILE="node-test-results-${RUNTIME}-${MODULE}.txt"

# Validate runtime
case "$RUNTIME" in
    edgebox|node|bun)
        ;;
    *)
        echo "Error: Unknown runtime '$RUNTIME'. Use: edgebox, node, or bun"
        exit 1
        ;;
esac

echo "=== Node.js Core Tests ($RUNTIME) ===" > "$RESULTS_FILE"
echo "Runtime: $RUNTIME" >> "$RESULTS_FILE"
echo "Module: $MODULE" >> "$RESULTS_FILE"
echo "Threads: $THREADS" >> "$RESULTS_FILE"
echo "Started: $(date)" >> "$RESULTS_FILE"
echo "" >> "$RESULTS_FILE"

# Create a temp directory for test builds
TEST_BUILD_DIR="/tmp/edgebox-node-tests-$$"
rm -rf "$TEST_BUILD_DIR"
mkdir -p "$TEST_BUILD_DIR"

# Create results directory for parallel test output
RESULTS_DIR="$TEST_BUILD_DIR/results"
mkdir -p "$RESULTS_DIR"

# Script to run a single test (called by xargs with test name only)
# Test code is read from $TEST_BUILD_DIR/tests/<test_name>.js
cat > "$TEST_BUILD_DIR/run_single_test.sh" << 'SCRIPT_EOF'
#!/bin/bash
set -e

test_name="$1"
test_code_file="$TEST_BUILD_DIR/tests/$test_name.js"

if [ ! -f "$test_code_file" ]; then
    echo "FAILED|$test_name|test file not found|" > "$RESULTS_DIR/$test_name.result"
    exit 0
fi

result_file="$RESULTS_DIR/$test_name.result"

# Runtime-specific execution
case "$RUNTIME" in
    node|bun)
        # Run directly with Node.js or Bun (no compilation needed)
        output=$(timeout 10s "$RUNTIME" "$test_code_file" 2>&1) || exit_code=$?
        ;;
    edgebox)
        # Compile and run with EdgeBox
        test_app_dir="$TEST_BUILD_DIR/$test_name"
        mkdir -p "$test_app_dir"
        cp "$test_code_file" "$test_app_dir/index.js"

        compile_output=$(./zig-out/bin/edgeboxc build "$test_app_dir" 2>&1) || compile_exit=$?

        if [ "${compile_exit:-0}" -ne 0 ]; then
            echo "FAILED|$test_name|compile failed|$(echo "$compile_output" | grep -E "error:" | head -1)" > "$result_file"
            rm -rf "$test_app_dir"
            exit 0
        fi

        # Find the compiled WASM - use the new per-project path structure
        wasm_file="./zig-out/bin/$test_app_dir/$test_name.wasm"
        if [ ! -f "$wasm_file" ]; then
            wasm_file="./zig-out/bin/$test_app_dir/$test_name.aot"
        fi

        if [ ! -f "$wasm_file" ]; then
            echo "FAILED|$test_name|no wasm output|" > "$result_file"
            rm -rf "$test_app_dir"
            exit 0
        fi

        output=$(timeout 10s ./zig-out/bin/edgebox "$wasm_file" 2>&1) || exit_code=$?

        # Cleanup EdgeBox artifacts
        rm -rf "$test_app_dir"
        rm -rf "./zig-out/bin/$test_app_dir"
        rm -rf "./zig-out/cache/$test_app_dir"
        ;;
esac

# Check results
if echo "$output" | grep -q "^SKIP:"; then
    echo "SKIPPED|$test_name||" > "$result_file"
elif echo "$output" | grep -q "PASS"; then
    echo "PASSED|$test_name||" > "$result_file"
elif [ ${exit_code:-0} -eq 0 ] && ! echo "$output" | grep -qE "(FAIL|Error|assert)"; then
    echo "PASSED|$test_name||" > "$result_file"
else
    echo "FAILED|$test_name||$(echo "$output" | head -2)" > "$result_file"
fi
SCRIPT_EOF

chmod +x "$TEST_BUILD_DIR/run_single_test.sh"

# Export variables for the test script
export TEST_BUILD_DIR RESULTS_DIR RUNTIME

# Create test directory for individual test files
TESTS_DIR="$TEST_BUILD_DIR/tests"
mkdir -p "$TESTS_DIR"

# Create test queue file (just test names, not code)
TEST_QUEUE="$TEST_BUILD_DIR/test_queue.txt"
> "$TEST_QUEUE"

echo "Queueing $MODULE tests..."

# Helper function to add a test
# Adds a polyfill for 'print' to work with Node.js/Bun (they use console.log)
add_test() {
    local name="$1"
    local code="$2"
    echo "$name" >> "$TEST_QUEUE"
    # Add print polyfill for Node.js/Bun compatibility
    echo "if (typeof print === 'undefined') { globalThis.print = console.log.bind(console); }" > "$TESTS_DIR/$name.js"
    echo "$code" >> "$TESTS_DIR/$name.js"
}

if [ "$MODULE" = "buffer" ] || [ "$MODULE" = "all" ]; then
    # Buffer tests - each test is written to its own file
    add_test "buffer-alloc" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.alloc(10); assert.strictEqual(b.length, 10); for (let i = 0; i < 10; i++) assert.strictEqual(b[i], 0); print("PASS: Buffer.alloc");'
    add_test "buffer-alloc-fill" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.alloc(5, 42); assert.strictEqual(b.length, 5); for (let i = 0; i < 5; i++) assert.strictEqual(b[i], 42); print("PASS: Buffer.alloc with fill");'
    add_test "buffer-allocUnsafe" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.allocUnsafe(10); assert.strictEqual(b.length, 10); print("PASS: Buffer.allocUnsafe");'
    add_test "buffer-from-string" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.from("hello"); assert.strictEqual(b.length, 5); assert.strictEqual(b.toString(), "hello"); print("PASS: Buffer.from string");'
    add_test "buffer-from-array" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.from([1, 2, 3, 4, 5]); assert.strictEqual(b.length, 5); assert.strictEqual(b[0], 1); assert.strictEqual(b[4], 5); print("PASS: Buffer.from array");'
    add_test "buffer-from-arraybuffer" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const ab = new ArrayBuffer(16); const view = new Uint8Array(ab); view[0] = 42; const b = Buffer.from(ab); assert.strictEqual(b.length, 16); assert.strictEqual(b[0], 42); print("PASS: Buffer.from ArrayBuffer");'
    add_test "buffer-concat" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b1 = Buffer.from([1, 2]); const b2 = Buffer.from([3, 4]); const b3 = Buffer.concat([b1, b2]); assert.strictEqual(b3.length, 4); assert.strictEqual(b3[0], 1); assert.strictEqual(b3[3], 4); print("PASS: Buffer.concat");'
    add_test "buffer-slice" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.from([1, 2, 3, 4, 5]); const s = b.slice(1, 4); assert.strictEqual(s.length, 3); assert.strictEqual(s[0], 2); assert.strictEqual(s[2], 4); print("PASS: Buffer.slice");'
    add_test "buffer-copy" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b1 = Buffer.from([1, 2, 3]); const b2 = Buffer.alloc(5); b1.copy(b2, 1); assert.strictEqual(b2[0], 0); assert.strictEqual(b2[1], 1); assert.strictEqual(b2[2], 2); assert.strictEqual(b2[3], 3); print("PASS: Buffer.copy");'
    add_test "buffer-fill" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.alloc(5); b.fill(255); for (let i = 0; i < 5; i++) assert.strictEqual(b[i], 255); print("PASS: Buffer.fill");'
    add_test "buffer-indexOf" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.from("hello world"); assert.strictEqual(b.indexOf("world"), 6); assert.strictEqual(b.indexOf("xyz"), -1); print("PASS: Buffer.indexOf");'
    add_test "buffer-includes" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.from("hello world"); assert.strictEqual(b.includes("world"), true); assert.strictEqual(b.includes("xyz"), false); print("PASS: Buffer.includes");'
    add_test "buffer-equals" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b1 = Buffer.from([1, 2, 3]); const b2 = Buffer.from([1, 2, 3]); const b3 = Buffer.from([1, 2, 4]); assert.strictEqual(b1.equals(b2), true); assert.strictEqual(b1.equals(b3), false); print("PASS: Buffer.equals");'
    add_test "buffer-compare" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b1 = Buffer.from([1, 2, 3]); const b2 = Buffer.from([1, 2, 3]); const b3 = Buffer.from([1, 2, 4]); assert.strictEqual(b1.compare(b2), 0); assert.strictEqual(b1.compare(b3), -1); assert.strictEqual(b3.compare(b1), 1); print("PASS: Buffer.compare");'
    add_test "buffer-write-read-int" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.alloc(8); b.writeInt32LE(12345, 0); assert.strictEqual(b.readInt32LE(0), 12345); b.writeInt32BE(-1000, 4); assert.strictEqual(b.readInt32BE(4), -1000); print("PASS: Buffer read/write int");'
    add_test "buffer-write-read-uint" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.alloc(8); b.writeUInt32LE(0xDEADBEEF, 0); assert.strictEqual(b.readUInt32LE(0), 0xDEADBEEF); b.writeUInt16BE(0xABCD, 4); assert.strictEqual(b.readUInt16BE(4), 0xABCD); print("PASS: Buffer read/write uint");'
    add_test "buffer-write-read-float" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.alloc(8); b.writeFloatLE(3.14, 0); assert(Math.abs(b.readFloatLE(0) - 3.14) < 0.001); b.writeDoubleLE(2.718281828, 0); assert(Math.abs(b.readDoubleLE(0) - 2.718281828) < 0.000001); print("PASS: Buffer read/write float");'
    add_test "buffer-toString-encoding" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.from("hello"); assert.strictEqual(b.toString("utf8"), "hello"); assert.strictEqual(b.toString("utf-8"), "hello"); print("PASS: Buffer.toString encoding");'
    add_test "buffer-isBuffer" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); assert.strictEqual(Buffer.isBuffer(Buffer.alloc(1)), true); assert.strictEqual(Buffer.isBuffer(new Uint8Array(1)), false); assert.strictEqual(Buffer.isBuffer("hello"), false); print("PASS: Buffer.isBuffer");'
    add_test "buffer-byteLength" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); assert.strictEqual(Buffer.byteLength("hello"), 5); assert.strictEqual(Buffer.byteLength("héllo"), 6); print("PASS: Buffer.byteLength");'
    add_test "buffer-toJSON" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.from([1, 2, 3]); const json = b.toJSON(); assert.strictEqual(json.type, "Buffer"); assert.deepStrictEqual(json.data, [1, 2, 3]); print("PASS: Buffer.toJSON");'
fi

if [ "$MODULE" = "path" ] || [ "$MODULE" = "all" ]; then
    add_test "path-join" 'const path = require("path"); const assert = require("assert"); assert.strictEqual(path.join("/foo", "bar", "baz"), "/foo/bar/baz"); assert.strictEqual(path.join("/foo", "../bar"), "/bar"); print("PASS: path.join");'
    add_test "path-resolve" 'const path = require("path"); const assert = require("assert"); const result = path.resolve("/foo/bar", "./baz"); assert.strictEqual(result, "/foo/bar/baz"); print("PASS: path.resolve");'
    add_test "path-dirname" 'const path = require("path"); const assert = require("assert"); assert.strictEqual(path.dirname("/foo/bar/baz.txt"), "/foo/bar"); assert.strictEqual(path.dirname("/foo/bar/"), "/foo"); print("PASS: path.dirname");'
    add_test "path-basename" 'const path = require("path"); const assert = require("assert"); assert.strictEqual(path.basename("/foo/bar/baz.txt"), "baz.txt"); assert.strictEqual(path.basename("/foo/bar/baz.txt", ".txt"), "baz"); print("PASS: path.basename");'
    add_test "path-extname" 'const path = require("path"); const assert = require("assert"); assert.strictEqual(path.extname("/foo/bar/baz.txt"), ".txt"); assert.strictEqual(path.extname("/foo/bar/baz"), ""); assert.strictEqual(path.extname("/foo/bar/.gitignore"), ""); print("PASS: path.extname");'
    add_test "path-isAbsolute" 'const path = require("path"); const assert = require("assert"); assert.strictEqual(path.isAbsolute("/foo/bar"), true); assert.strictEqual(path.isAbsolute("foo/bar"), false); print("PASS: path.isAbsolute");'
    add_test "path-normalize" 'const path = require("path"); const assert = require("assert"); assert.strictEqual(path.normalize("/foo/bar//baz/.."), "/foo/bar"); print("PASS: path.normalize");'
    add_test "path-parse" 'const path = require("path"); const assert = require("assert"); const p = path.parse("/home/user/file.txt"); assert.strictEqual(p.root, "/"); assert.strictEqual(p.dir, "/home/user"); assert.strictEqual(p.base, "file.txt"); assert.strictEqual(p.ext, ".txt"); assert.strictEqual(p.name, "file"); print("PASS: path.parse");'
    add_test "path-format" 'const path = require("path"); const assert = require("assert"); const p = path.format({ root: "/", dir: "/home/user", base: "file.txt" }); assert.strictEqual(p, "/home/user/file.txt"); print("PASS: path.format");'
    add_test "path-sep" 'const path = require("path"); const assert = require("assert"); assert.strictEqual(path.sep, "/"); print("PASS: path.sep");'
fi

if [ "$MODULE" = "fs" ] || [ "$MODULE" = "all" ]; then
    add_test "fs-existsSync" 'const fs = require("fs"); const assert = require("assert"); assert.strictEqual(fs.existsSync("/tmp"), true); assert.strictEqual(fs.existsSync("/nonexistent-path-12345"), false); print("PASS: fs.existsSync");'
    add_test "fs-readFileSync-writeFileSync" 'const fs = require("fs"); const assert = require("assert"); const testFile = "/tmp/edgebox-test-" + Date.now() + ".txt"; fs.writeFileSync(testFile, "hello world"); const content = fs.readFileSync(testFile, "utf8"); assert.strictEqual(content, "hello world"); fs.unlinkSync(testFile); print("PASS: fs.readFileSync/writeFileSync");'
    add_test "fs-readdirSync" 'const fs = require("fs"); const assert = require("assert"); const files = fs.readdirSync("/tmp"); assert(Array.isArray(files)); print("PASS: fs.readdirSync");'
    add_test "fs-statSync" 'const fs = require("fs"); const assert = require("assert"); const stat = fs.statSync("/tmp"); assert(stat.isDirectory()); assert(!stat.isFile()); print("PASS: fs.statSync");'
    add_test "fs-mkdirSync-rmdirSync" 'const fs = require("fs"); const assert = require("assert"); const testDir = "/tmp/edgebox-test-dir-" + Date.now(); fs.mkdirSync(testDir); assert(fs.existsSync(testDir)); fs.rmdirSync(testDir); assert(!fs.existsSync(testDir)); print("PASS: fs.mkdirSync/rmdirSync");'
fi

if [ "$MODULE" = "crypto" ] || [ "$MODULE" = "all" ]; then
    add_test "crypto-randomBytes" 'const crypto = require("crypto"); const assert = require("assert"); const bytes = crypto.randomBytes(16); assert.strictEqual(bytes.length, 16); print("PASS: crypto.randomBytes");'
    add_test "crypto-createHash-sha256" 'const crypto = require("crypto"); const assert = require("assert"); const hash = crypto.createHash("sha256"); hash.update("hello"); const digest = hash.digest("hex"); assert.strictEqual(digest, "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"); print("PASS: crypto.createHash sha256");'
    add_test "crypto-createHash-md5" 'const crypto = require("crypto"); const assert = require("assert"); const hash = crypto.createHash("md5"); hash.update("hello"); const digest = hash.digest("hex"); assert.strictEqual(digest, "5d41402abc4b2a76b9719d911017c592"); print("PASS: crypto.createHash md5");'
    add_test "crypto-getRandomValues" 'const assert = require("assert"); const arr = new Uint8Array(16); crypto.getRandomValues(arr); let hasNonZero = false; for (let i = 0; i < arr.length; i++) if (arr[i] !== 0) hasNonZero = true; assert(hasNonZero); print("PASS: crypto.getRandomValues");'
    add_test "crypto-randomUUID" 'const assert = require("assert"); const uuid = crypto.randomUUID(); assert(typeof uuid === "string"); assert.strictEqual(uuid.length, 36); assert(uuid.match(/^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/)); print("PASS: crypto.randomUUID");'
fi

if [ "$MODULE" = "util" ] || [ "$MODULE" = "all" ]; then
    add_test "util-inspect" 'const util = require("util"); const assert = require("assert"); const result = util.inspect({ a: 1, b: "hello" }); assert(typeof result === "string"); assert(result.includes("a")); print("PASS: util.inspect");'
    add_test "util-format" 'const util = require("util"); const assert = require("assert"); assert.strictEqual(util.format("Hello %s", "World"), "Hello World"); assert.strictEqual(util.format("Number: %d", 42), "Number: 42"); print("PASS: util.format");'
    add_test "util-promisify" 'const util = require("util"); const assert = require("assert"); function callbackFn(arg, cb) { cb(null, arg + 1); } const promiseFn = util.promisify(callbackFn); promiseFn(5).then(result => { assert.strictEqual(result, 6); print("PASS: util.promisify"); }).catch(e => print("FAIL:", e.message));'
    add_test "util-types" 'const util = require("util"); const assert = require("assert"); assert.strictEqual(util.types.isDate(new Date()), true); assert.strictEqual(util.types.isDate("2023-01-01"), false); assert.strictEqual(util.types.isRegExp(/abc/), true); assert.strictEqual(util.types.isRegExp("abc"), false); print("PASS: util.types");'
fi

if [ "$MODULE" = "process" ] || [ "$MODULE" = "all" ]; then
    add_test "process-env" 'const assert = require("assert"); assert(typeof process.env === "object"); print("PASS: process.env");'
    add_test "process-cwd" 'const assert = require("assert"); const cwd = process.cwd(); assert(typeof cwd === "string"); assert(cwd.startsWith("/")); print("PASS: process.cwd");'
    add_test "process-platform" 'const assert = require("assert"); assert(["darwin", "linux", "win32"].includes(process.platform)); print("PASS: process.platform");'
    add_test "process-arch" 'const assert = require("assert"); assert(["arm64", "x64", "ia32", "arm"].includes(process.arch)); print("PASS: process.arch");'
    add_test "process-version" 'const assert = require("assert"); assert(typeof process.version === "string"); assert(process.version.startsWith("v")); print("PASS: process.version");'
    add_test "process-argv" 'const assert = require("assert"); assert(Array.isArray(process.argv)); print("PASS: process.argv");'
fi

if [ "$MODULE" = "events" ] || [ "$MODULE" = "all" ]; then
    add_test "events-on-emit" 'const EventEmitter = require("events"); const assert = require("assert"); const ee = new EventEmitter(); let called = false; ee.on("test", () => { called = true; }); ee.emit("test"); assert.strictEqual(called, true); print("PASS: EventEmitter on/emit");'
    add_test "events-once" 'const EventEmitter = require("events"); const assert = require("assert"); const ee = new EventEmitter(); let count = 0; ee.once("test", () => { count++; }); ee.emit("test"); ee.emit("test"); assert.strictEqual(count, 1); print("PASS: EventEmitter once");'
    add_test "events-removeListener" 'const EventEmitter = require("events"); const assert = require("assert"); const ee = new EventEmitter(); let count = 0; const handler = () => { count++; }; ee.on("test", handler); ee.emit("test"); ee.removeListener("test", handler); ee.emit("test"); assert.strictEqual(count, 1); print("PASS: EventEmitter removeListener");'
    add_test "events-listenerCount" 'const EventEmitter = require("events"); const assert = require("assert"); const ee = new EventEmitter(); ee.on("test", () => {}); ee.on("test", () => {}); assert.strictEqual(ee.listenerCount("test"), 2); print("PASS: EventEmitter listenerCount");'
fi

if [ "$MODULE" = "timers" ] || [ "$MODULE" = "all" ]; then
    add_test "timers-setTimeout" 'const assert = require("assert"); let called = false; setTimeout(() => { called = true; assert.strictEqual(called, true); print("PASS: setTimeout"); }, 10);'
    add_test "timers-clearTimeout" 'const assert = require("assert"); let called = false; const id = setTimeout(() => { called = true; }, 50); clearTimeout(id); setTimeout(() => { assert.strictEqual(called, false); print("PASS: clearTimeout"); }, 100);'
    add_test "timers-setInterval" 'const assert = require("assert"); let count = 0; const id = setInterval(() => { count++; if (count >= 3) { clearInterval(id); assert.strictEqual(count, 3); print("PASS: setInterval"); } }, 10);'
fi

# Count total tests
TOTAL_TESTS=$(wc -l < "$TEST_QUEUE" | tr -d ' ')
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
echo "=== $RUNTIME $MODULE Tests Summary ==="
echo "Runtime: $RUNTIME"
echo "Passed:  $PASSED"
echo "Failed:  $FAILED"
echo "Skipped: $SKIPPED"
echo "Pass Rate: $PASS_RATE"

# Exit with success (we want to track progress, not block CI)
exit 0
