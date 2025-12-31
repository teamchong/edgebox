#!/bin/bash
# Run npm Package Compatibility Tests on EdgeBox
# Usage: ./scripts/run-npm-compat-tests.sh <package>
#
# Tests that popular npm packages work correctly with EdgeBox.
# Each package has a set of smoke tests that verify core functionality.

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

PACKAGE="${1:-lodash}"

# Determine runtime and output file
if [ "$NODE_TEST" = "1" ]; then
    RUNTIME="node"
    RUNTIME_CMD="node"
    RESULTS_FILE="npm-compat-results-${PACKAGE}-node.txt"
elif [ "$BUN_TEST" = "1" ]; then
    RUNTIME="bun"
    RUNTIME_CMD="bun"
    RESULTS_FILE="npm-compat-results-${PACKAGE}-bun.txt"
else
    RUNTIME="edgebox"
    RUNTIME_CMD="./zig-out/bin/edgebox"
    RESULTS_FILE="npm-compat-results-${PACKAGE}.txt"
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

echo "=== npm Package Tests for $RUNTIME ===" > "$RESULTS_FILE"
echo "Package: $PACKAGE" >> "$RESULTS_FILE"
echo "Started: $(date)" >> "$RESULTS_FILE"
echo "" >> "$RESULTS_FILE"

# Test directory where npm install was run
NPM_TEST_DIR="test-npm/$PACKAGE"

# Function to run a single test
run_test() {
    local test_name="$1"
    local test_code="$2"

    # Create test file with print() polyfill for Node.js compatibility
    # (EdgeBox has print() built-in, Node.js needs console.log)
    cat > "$NPM_TEST_DIR/index.js" << 'POLYFILL'
if (typeof print === 'undefined') { globalThis.print = console.log; }
POLYFILL
    echo "$test_code" >> "$NPM_TEST_DIR/index.js"

    local output
    local exit_code=0
    local compile_time_ms=0
    local run_time_ms=0

    if [ "$RUNTIME" = "edgebox" ]; then
        # Compile first
        local compile_output
        local compile_exit=0
        local compile_start=$(get_time_ms)
        compile_output=$(./zig-out/bin/edgeboxc build "$NPM_TEST_DIR" 2>&1) || compile_exit=$?
        local compile_end=$(get_time_ms)
        compile_time_ms=$((compile_end - compile_start))
        TOTAL_COMPILE_TIME=$((TOTAL_COMPILE_TIME + compile_time_ms))

        if [ $compile_exit -ne 0 ]; then
            echo "✗ $test_name (compile failed)" >> "$RESULTS_FILE"
            echo "  Error: $(echo "$compile_output" | grep -E "error:" | head -1)" >> "$RESULTS_FILE"
            FAILED=$((FAILED + 1))
            return
        fi

        # Find compiled output - handle the path structure
        local wasm_file="./zig-out/bin/test-npm/$PACKAGE/$PACKAGE.aot"
        if [ ! -f "$wasm_file" ]; then
            wasm_file="./zig-out/bin/test-npm/$PACKAGE/$PACKAGE.wasm"
        fi

        if [ ! -f "$wasm_file" ]; then
            echo "✗ $test_name (no wasm output)" >> "$RESULTS_FILE"
            FAILED=$((FAILED + 1))
            return
        fi

        local run_start=$(get_time_ms)
        output=$(run_with_timeout 30 ./zig-out/bin/edgebox "$wasm_file" 2>&1) || exit_code=$?
        local run_end=$(get_time_ms)
        run_time_ms=$((run_end - run_start))
        TOTAL_RUN_TIME=$((TOTAL_RUN_TIME + run_time_ms))
    else
        local run_start=$(get_time_ms)
        # Run from package directory so relative paths work (test-files/, .env, etc.)
        output=$(cd "$NPM_TEST_DIR" && run_with_timeout 30 $RUNTIME_CMD index.js 2>&1) || exit_code=$?
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
}

echo "Running $PACKAGE tests..."
echo ""

case "$PACKAGE" in
    lodash)
        run_test "lodash-chunk" '
const _ = require("lodash");
const assert = require("assert");
const result = _.chunk([1, 2, 3, 4, 5], 2);
assert.deepStrictEqual(result, [[1, 2], [3, 4], [5]]);
print("PASS: lodash chunk");
'

        run_test "lodash-flatten" '
const _ = require("lodash");
const assert = require("assert");
const result = _.flatten([[1], [2, 3], [4]]);
assert.deepStrictEqual(result, [1, 2, 3, 4]);
print("PASS: lodash flatten");
'

        run_test "lodash-groupby" '
const _ = require("lodash");
const assert = require("assert");
const result = _.groupBy([6.1, 4.2, 6.3], Math.floor);
assert.deepStrictEqual(result, { "4": [4.2], "6": [6.1, 6.3] });
print("PASS: lodash groupBy");
'

        run_test "lodash-debounce" '
const _ = require("lodash");
const assert = require("assert");
let count = 0;
const debounced = _.debounce(() => count++, 100);
debounced();
debounced();
debounced();
assert.strictEqual(count, 0); // Should not have fired yet
print("PASS: lodash debounce");
'

        run_test "lodash-clonedeep" '
const _ = require("lodash");
const assert = require("assert");
const obj = { a: { b: 2 } };
const clone = _.cloneDeep(obj);
clone.a.b = 3;
assert.strictEqual(obj.a.b, 2); // Original unchanged
assert.strictEqual(clone.a.b, 3);
print("PASS: lodash cloneDeep");
'
        ;;

    dotenv)
        # Create a .env file for testing
        echo 'TEST_VAR=hello_world
ANOTHER_VAR=42' > "$NPM_TEST_DIR/.env"

        run_test "dotenv-config" '
const dotenv = require("dotenv");
const assert = require("assert");
const result = dotenv.config();
assert.strictEqual(result.error, undefined);
assert.strictEqual(process.env.TEST_VAR, "hello_world");
assert.strictEqual(process.env.ANOTHER_VAR, "42");
print("PASS: dotenv config");
'

        run_test "dotenv-parse" '
const dotenv = require("dotenv");
const assert = require("assert");
const parsed = dotenv.parse("FOO=bar\nBAZ=qux");
assert.strictEqual(parsed.FOO, "bar");
assert.strictEqual(parsed.BAZ, "qux");
print("PASS: dotenv parse");
'
        ;;

    commander)
        run_test "commander-basic" '
const { Command } = require("commander");
const assert = require("assert");
const program = new Command();
program
  .option("-d, --debug", "enable debug mode")
  .option("-p, --port <number>", "port number", "3000");
program.parse(["node", "test", "-d", "-p", "8080"]);
const opts = program.opts();
assert.strictEqual(opts.debug, true);
assert.strictEqual(opts.port, "8080");
print("PASS: commander basic");
'

        run_test "commander-subcommand" '
const { Command } = require("commander");
const assert = require("assert");
const program = new Command();
let subCalled = false;
program
  .command("start")
  .action(() => { subCalled = true; });
program.parse(["node", "test", "start"]);
assert.strictEqual(subCalled, true);
print("PASS: commander subcommand");
'
        ;;

    chalk)
        run_test "chalk-basic" '
const assert = require("assert");
const chalk = require("chalk");
const result = chalk.red("hello");
assert.strictEqual(typeof result, "string");
assert.ok(result.includes("hello"));
print("PASS: chalk basic");
'

        run_test "chalk-chain" '
const assert = require("assert");
const chalk = require("chalk");
const result = chalk.bold.blue("styled");
assert.strictEqual(typeof result, "string");
assert.ok(result.includes("styled"));
print("PASS: chalk chain");
'
        ;;

    uuid)
        run_test "uuid-v4" '
const assert = require("assert");
const { v4: uuidv4 } = require("uuid");
const id = uuidv4();
assert.strictEqual(typeof id, "string");
assert.strictEqual(id.length, 36);
assert.ok(/^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i.test(id));
print("PASS: uuid v4");
'

        run_test "uuid-validate" '
const assert = require("assert");
const { validate } = require("uuid");
assert.strictEqual(validate("9b1deb4d-3b7d-4bad-9bdd-2b0d7b3dcb6d"), true);
assert.strictEqual(validate("not-a-uuid"), false);
print("PASS: uuid validate");
'
        ;;

    semver)
        run_test "semver-valid" '
const semver = require("semver");
const assert = require("assert");
assert.strictEqual(semver.valid("1.2.3"), "1.2.3");
assert.strictEqual(semver.valid("v1.2.3"), "1.2.3");
assert.strictEqual(semver.valid("invalid"), null);
print("PASS: semver valid");
'

        run_test "semver-compare" '
const semver = require("semver");
const assert = require("assert");
assert.strictEqual(semver.gt("1.2.3", "1.2.2"), true);
assert.strictEqual(semver.lt("1.2.3", "1.3.0"), true);
assert.strictEqual(semver.eq("1.2.3", "1.2.3"), true);
print("PASS: semver compare");
'

        run_test "semver-satisfies" '
const semver = require("semver");
const assert = require("assert");
assert.strictEqual(semver.satisfies("1.2.3", "^1.0.0"), true);
assert.strictEqual(semver.satisfies("2.0.0", "^1.0.0"), false);
assert.strictEqual(semver.satisfies("1.5.0", ">=1.0.0 <2.0.0"), true);
print("PASS: semver satisfies");
'
        ;;

    minimist)
        run_test "minimist-basic" '
const minimist = require("minimist");
const assert = require("assert");
const args = minimist(["--name", "value", "-x", "3", "-y", "4"]);
assert.strictEqual(args.name, "value");
assert.strictEqual(args.x, 3);
assert.strictEqual(args.y, 4);
print("PASS: minimist basic");
'

        run_test "minimist-boolean" '
const minimist = require("minimist");
const assert = require("assert");
const args = minimist(["--verbose", "--no-cache"]);
assert.strictEqual(args.verbose, true);
assert.strictEqual(args.cache, false);
print("PASS: minimist boolean");
'
        ;;

    glob)
        # Create test files for glob
        mkdir -p "$NPM_TEST_DIR/test-files"
        touch "$NPM_TEST_DIR/test-files/a.js"
        touch "$NPM_TEST_DIR/test-files/b.js"
        touch "$NPM_TEST_DIR/test-files/c.txt"

        run_test "glob-sync" '
const { globSync } = require("glob");
const assert = require("assert");
const files = globSync("test-files/*.js");
assert.strictEqual(files.length, 2);
assert.ok(files.some(f => f.includes("a.js")));
assert.ok(files.some(f => f.includes("b.js")));
print("PASS: glob sync");
'

        run_test "glob-async" '
const { glob } = require("glob");
const assert = require("assert");
glob("test-files/*.js").then(files => {
    assert.strictEqual(files.length, 2);
    print("PASS: glob async");
});
'
        ;;

    yaml)
        run_test "yaml-parse" '
const YAML = require("yaml");
const assert = require("assert");
const doc = YAML.parse("name: test\nvalue: 42");
assert.strictEqual(doc.name, "test");
assert.strictEqual(doc.value, 42);
print("PASS: yaml parse");
'

        run_test "yaml-stringify" '
const YAML = require("yaml");
const assert = require("assert");
const result = YAML.stringify({ name: "test", value: 42 });
assert.ok(result.includes("name: test"));
assert.ok(result.includes("value: 42"));
print("PASS: yaml stringify");
'

        run_test "yaml-array" '
const YAML = require("yaml");
const assert = require("assert");
const doc = YAML.parse("items:\n  - a\n  - b\n  - c");
assert.deepStrictEqual(doc.items, ["a", "b", "c"]);
print("PASS: yaml array");
'
        ;;

    dayjs)
        run_test "dayjs-basic" '
const dayjs = require("dayjs");
const assert = require("assert");
const date = dayjs("2024-01-15");
assert.strictEqual(date.year(), 2024);
assert.strictEqual(date.month(), 0); // 0-indexed
assert.strictEqual(date.date(), 15);
print("PASS: dayjs basic");
'

        run_test "dayjs-format" '
const dayjs = require("dayjs");
const assert = require("assert");
const date = dayjs("2024-01-15");
assert.strictEqual(date.format("YYYY-MM-DD"), "2024-01-15");
print("PASS: dayjs format");
'

        run_test "dayjs-manipulation" '
const dayjs = require("dayjs");
const assert = require("assert");
const date = dayjs("2024-01-15");
const nextWeek = date.add(7, "day");
assert.strictEqual(nextWeek.date(), 22);
print("PASS: dayjs manipulation");
'

        run_test "dayjs-compare" '
const dayjs = require("dayjs");
const assert = require("assert");
const date1 = dayjs("2024-01-15");
const date2 = dayjs("2024-01-20");
assert.strictEqual(date1.isBefore(date2), true);
assert.strictEqual(date2.isAfter(date1), true);
print("PASS: dayjs compare");
'
        ;;

    *)
        echo "Unknown package: $PACKAGE" >> "$RESULTS_FILE"
        FAILED=1
        ;;
esac

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
echo "=== npm $PACKAGE Results ($RUNTIME) ==="
echo "Passed: $PASSED"
echo "Failed: $FAILED"
echo "Skipped: $SKIPPED"
if [ "$RUNTIME" = "edgebox" ]; then
    echo "Compile time: ${TOTAL_COMPILE_TIME}ms"
fi
echo "Run time: ${TOTAL_RUN_TIME}ms"
echo ""

# Exit with failure if any tests failed
if [ $FAILED -gt 0 ]; then
    exit 1
fi
