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

TESTS_DIR="$TEST_BUILD_DIR/tests"
mkdir -p "$TESTS_DIR"

# Script to run a single test (called by xargs with test name only)
cat > "$TEST_BUILD_DIR/run_single_test.sh" << 'SCRIPT_EOF'
#!/bin/bash
set -e

test_name="$1"
test_file="$TESTS_DIR/$test_name.js"
result_file="$RESULTS_DIR/$test_name.result"

if [ ! -f "$test_file" ]; then
    echo "FAILED|$test_name|test file not found|" > "$result_file"
    exit 0
fi

output=""
exit_code=0

if [ "$MODE" = "compile" ] && [ "$ENGINE" = "bun" ]; then
    # Bun: compile to binary then run
    binary_file="$TEST_BUILD_DIR/$test_name-bin"
    if ! bun build --compile "$test_file" --outfile "$binary_file" > /dev/null 2>&1; then
        echo "FAILED|$test_name|compile failed|" > "$result_file"
        rm -f "$binary_file"
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
SCRIPT_EOF

chmod +x "$TEST_BUILD_DIR/run_single_test.sh"

# Export variables for the test script
export TEST_BUILD_DIR RESULTS_DIR TESTS_DIR ENGINE MODE

# Create test queue file (just test names, not code)
TEST_QUEUE="$TEST_BUILD_DIR/test_queue.txt"
> "$TEST_QUEUE"

echo "Queueing $MODULE tests..."

# Helper function to add a test
add_test() {
    local name="$1"
    local code="$2"
    echo "$name" >> "$TEST_QUEUE"
    echo "$code" > "$TESTS_DIR/$name.js"
}

if [ "$MODULE" = "buffer" ] || [ "$MODULE" = "all" ]; then
    add_test "buffer-alloc" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.alloc(10); assert.strictEqual(b.length, 10); for (let i = 0; i < 10; i++) assert.strictEqual(b[i], 0); console.log("PASS: Buffer.alloc");'
    add_test "buffer-alloc-fill" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.alloc(5, 42); assert.strictEqual(b.length, 5); for (let i = 0; i < 5; i++) assert.strictEqual(b[i], 42); console.log("PASS: Buffer.alloc with fill");'
    add_test "buffer-allocUnsafe" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.allocUnsafe(10); assert.strictEqual(b.length, 10); console.log("PASS: Buffer.allocUnsafe");'
    add_test "buffer-from-string" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.from("hello"); assert.strictEqual(b.length, 5); assert.strictEqual(b.toString(), "hello"); console.log("PASS: Buffer.from string");'
    add_test "buffer-from-array" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.from([1, 2, 3, 4, 5]); assert.strictEqual(b.length, 5); assert.strictEqual(b[0], 1); assert.strictEqual(b[4], 5); console.log("PASS: Buffer.from array");'
    add_test "buffer-from-arraybuffer" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const ab = new ArrayBuffer(16); const view = new Uint8Array(ab); view[0] = 42; const b = Buffer.from(ab); assert.strictEqual(b.length, 16); assert.strictEqual(b[0], 42); console.log("PASS: Buffer.from ArrayBuffer");'
    add_test "buffer-concat" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b1 = Buffer.from([1, 2]); const b2 = Buffer.from([3, 4]); const b3 = Buffer.concat([b1, b2]); assert.strictEqual(b3.length, 4); assert.strictEqual(b3[0], 1); assert.strictEqual(b3[3], 4); console.log("PASS: Buffer.concat");'
    add_test "buffer-slice" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.from([1, 2, 3, 4, 5]); const s = b.slice(1, 4); assert.strictEqual(s.length, 3); assert.strictEqual(s[0], 2); assert.strictEqual(s[2], 4); console.log("PASS: Buffer.slice");'
    add_test "buffer-copy" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b1 = Buffer.from([1, 2, 3]); const b2 = Buffer.alloc(5); b1.copy(b2, 1); assert.strictEqual(b2[0], 0); assert.strictEqual(b2[1], 1); assert.strictEqual(b2[2], 2); assert.strictEqual(b2[3], 3); console.log("PASS: Buffer.copy");'
    add_test "buffer-fill" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.alloc(5); b.fill(255); for (let i = 0; i < 5; i++) assert.strictEqual(b[i], 255); console.log("PASS: Buffer.fill");'
    add_test "buffer-indexOf" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.from("hello world"); assert.strictEqual(b.indexOf("world"), 6); assert.strictEqual(b.indexOf("xyz"), -1); console.log("PASS: Buffer.indexOf");'
    add_test "buffer-includes" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.from("hello world"); assert.strictEqual(b.includes("world"), true); assert.strictEqual(b.includes("xyz"), false); console.log("PASS: Buffer.includes");'
    add_test "buffer-equals" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b1 = Buffer.from([1, 2, 3]); const b2 = Buffer.from([1, 2, 3]); const b3 = Buffer.from([1, 2, 4]); assert.strictEqual(b1.equals(b2), true); assert.strictEqual(b1.equals(b3), false); console.log("PASS: Buffer.equals");'
    add_test "buffer-compare" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b1 = Buffer.from([1, 2, 3]); const b2 = Buffer.from([1, 2, 3]); const b3 = Buffer.from([1, 2, 4]); assert.strictEqual(b1.compare(b2), 0); assert.strictEqual(b1.compare(b3), -1); assert.strictEqual(b3.compare(b1), 1); console.log("PASS: Buffer.compare");'
    add_test "buffer-write-read-int" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.alloc(8); b.writeInt32LE(12345, 0); assert.strictEqual(b.readInt32LE(0), 12345); b.writeInt32BE(-1000, 4); assert.strictEqual(b.readInt32BE(4), -1000); console.log("PASS: Buffer read/write int");'
    add_test "buffer-write-read-uint" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.alloc(8); b.writeUInt32LE(0xDEADBEEF, 0); assert.strictEqual(b.readUInt32LE(0), 0xDEADBEEF); b.writeUInt16BE(0xABCD, 4); assert.strictEqual(b.readUInt16BE(4), 0xABCD); console.log("PASS: Buffer read/write uint");'
    add_test "buffer-write-read-float" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.alloc(8); b.writeFloatLE(3.14, 0); assert(Math.abs(b.readFloatLE(0) - 3.14) < 0.001); b.writeDoubleLE(2.718281828, 0); assert(Math.abs(b.readDoubleLE(0) - 2.718281828) < 0.000001); console.log("PASS: Buffer read/write float");'
    add_test "buffer-toString-encoding" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.from("hello"); assert.strictEqual(b.toString("utf8"), "hello"); assert.strictEqual(b.toString("utf-8"), "hello"); console.log("PASS: Buffer.toString encoding");'
    add_test "buffer-isBuffer" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); assert.strictEqual(Buffer.isBuffer(Buffer.alloc(1)), true); assert.strictEqual(Buffer.isBuffer(new Uint8Array(1)), false); assert.strictEqual(Buffer.isBuffer("hello"), false); console.log("PASS: Buffer.isBuffer");'
    add_test "buffer-byteLength" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); assert.strictEqual(Buffer.byteLength("hello"), 5); assert.strictEqual(Buffer.byteLength("héllo"), 6); console.log("PASS: Buffer.byteLength");'
    add_test "buffer-toJSON" 'const Buffer = require("buffer").Buffer; const assert = require("assert"); const b = Buffer.from([1, 2, 3]); const json = b.toJSON(); assert.strictEqual(json.type, "Buffer"); assert.deepStrictEqual(json.data, [1, 2, 3]); console.log("PASS: Buffer.toJSON");'
fi

if [ "$MODULE" = "path" ] || [ "$MODULE" = "all" ]; then
    add_test "path-join" 'const path = require("path"); const assert = require("assert"); assert.strictEqual(path.join("/foo", "bar", "baz"), "/foo/bar/baz"); assert.strictEqual(path.join("/foo", "../bar"), "/bar"); console.log("PASS: path.join");'
    add_test "path-resolve" 'const path = require("path"); const assert = require("assert"); const result = path.resolve("/foo/bar", "./baz"); assert.strictEqual(result, "/foo/bar/baz"); console.log("PASS: path.resolve");'
    add_test "path-dirname" 'const path = require("path"); const assert = require("assert"); assert.strictEqual(path.dirname("/foo/bar/baz.txt"), "/foo/bar"); assert.strictEqual(path.dirname("/foo/bar/"), "/foo"); console.log("PASS: path.dirname");'
    add_test "path-basename" 'const path = require("path"); const assert = require("assert"); assert.strictEqual(path.basename("/foo/bar/baz.txt"), "baz.txt"); assert.strictEqual(path.basename("/foo/bar/baz.txt", ".txt"), "baz"); console.log("PASS: path.basename");'
    add_test "path-extname" 'const path = require("path"); const assert = require("assert"); assert.strictEqual(path.extname("/foo/bar/baz.txt"), ".txt"); assert.strictEqual(path.extname("/foo/bar/baz"), ""); assert.strictEqual(path.extname("/foo/bar/.gitignore"), ""); console.log("PASS: path.extname");'
    add_test "path-isAbsolute" 'const path = require("path"); const assert = require("assert"); assert.strictEqual(path.isAbsolute("/foo/bar"), true); assert.strictEqual(path.isAbsolute("foo/bar"), false); console.log("PASS: path.isAbsolute");'
    add_test "path-normalize" 'const path = require("path"); const assert = require("assert"); assert.strictEqual(path.normalize("/foo/bar//baz/.."), "/foo/bar"); console.log("PASS: path.normalize");'
    add_test "path-parse" 'const path = require("path"); const assert = require("assert"); const p = path.parse("/home/user/file.txt"); assert.strictEqual(p.root, "/"); assert.strictEqual(p.dir, "/home/user"); assert.strictEqual(p.base, "file.txt"); assert.strictEqual(p.ext, ".txt"); assert.strictEqual(p.name, "file"); console.log("PASS: path.parse");'
    add_test "path-format" 'const path = require("path"); const assert = require("assert"); const p = path.format({ root: "/", dir: "/home/user", base: "file.txt" }); assert.strictEqual(p, "/home/user/file.txt"); console.log("PASS: path.format");'
    add_test "path-sep" 'const path = require("path"); const assert = require("assert"); assert.strictEqual(path.sep, "/"); console.log("PASS: path.sep");'
fi

if [ "$MODULE" = "fs" ] || [ "$MODULE" = "all" ]; then
    add_test "fs-existsSync" 'const fs = require("fs"); const assert = require("assert"); assert.strictEqual(fs.existsSync("/tmp"), true); assert.strictEqual(fs.existsSync("/nonexistent-path-12345"), false); console.log("PASS: fs.existsSync");'
    add_test "fs-readFileSync-writeFileSync" 'const fs = require("fs"); const assert = require("assert"); const testFile = "/tmp/edgebox-test-" + Date.now() + ".txt"; fs.writeFileSync(testFile, "hello world"); const content = fs.readFileSync(testFile, "utf8"); assert.strictEqual(content, "hello world"); fs.unlinkSync(testFile); console.log("PASS: fs.readFileSync/writeFileSync");'
    add_test "fs-readdirSync" 'const fs = require("fs"); const assert = require("assert"); const files = fs.readdirSync("/tmp"); assert(Array.isArray(files)); console.log("PASS: fs.readdirSync");'
    add_test "fs-statSync" 'const fs = require("fs"); const assert = require("assert"); const stat = fs.statSync("/tmp"); assert(stat.isDirectory()); assert(!stat.isFile()); console.log("PASS: fs.statSync");'
    add_test "fs-mkdirSync-rmdirSync" 'const fs = require("fs"); const assert = require("assert"); const testDir = "/tmp/edgebox-test-dir-" + Date.now(); fs.mkdirSync(testDir); assert(fs.existsSync(testDir)); fs.rmdirSync(testDir); assert(!fs.existsSync(testDir)); console.log("PASS: fs.mkdirSync/rmdirSync");'
fi

if [ "$MODULE" = "crypto" ] || [ "$MODULE" = "all" ]; then
    add_test "crypto-randomBytes" 'const crypto = require("crypto"); const assert = require("assert"); const bytes = crypto.randomBytes(16); assert.strictEqual(bytes.length, 16); console.log("PASS: crypto.randomBytes");'
    add_test "crypto-createHash-sha256" 'const crypto = require("crypto"); const assert = require("assert"); const hash = crypto.createHash("sha256"); hash.update("hello"); const digest = hash.digest("hex"); assert.strictEqual(digest, "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"); console.log("PASS: crypto.createHash sha256");'
    add_test "crypto-createHash-md5" 'const crypto = require("crypto"); const assert = require("assert"); const hash = crypto.createHash("md5"); hash.update("hello"); const digest = hash.digest("hex"); assert.strictEqual(digest, "5d41402abc4b2a76b9719d911017c592"); console.log("PASS: crypto.createHash md5");'
    add_test "crypto-getRandomValues" 'const assert = require("assert"); const arr = new Uint8Array(16); crypto.getRandomValues(arr); let hasNonZero = false; for (let i = 0; i < arr.length; i++) if (arr[i] !== 0) hasNonZero = true; assert(hasNonZero); console.log("PASS: crypto.getRandomValues");'
    add_test "crypto-randomUUID" 'const assert = require("assert"); const uuid = crypto.randomUUID(); assert(typeof uuid === "string"); assert.strictEqual(uuid.length, 36); assert(uuid.match(/^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/)); console.log("PASS: crypto.randomUUID");'
fi

# Count total tests
TOTAL_TESTS=$(wc -l < "$TEST_QUEUE" | tr -d ' ')
echo "Running $TOTAL_TESTS tests in parallel with $THREADS threads..."
echo ""

# Run tests in parallel using xargs with line-by-line processing
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
