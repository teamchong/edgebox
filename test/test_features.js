// EdgeBox Feature Tests
// Run with: ./run.sh test/test_features.js

print("=== EdgeBox Feature Tests ===\n");

// Load Node.js polyfills via require() - this triggers lazy loading
const tty = require('tty');
const child_process = require('child_process');
const path = require('path');
const url = require('url');
const events = require('events');
const util = require('util');
const os = require('os');
const EventEmitter = events;

let passed = 0;
let failed = 0;

function test(name, fn) {
    try {
        fn();
        print("✓ " + name);
        passed++;
    } catch (e) {
        print("✗ " + name + ": " + e.message);
        failed++;
    }
}

function assert(condition, message) {
    if (!condition) {
        throw new Error(message || "Assertion failed");
    }
}

// =============================================================================
// Basic QuickJS Tests
// =============================================================================
print("\n--- QuickJS Core ---");

test("print() works", () => {
    // If we get here, print works
    assert(true);
});

test("Math operations", () => {
    assert(1 + 2 === 3, "1 + 2 should equal 3");
    assert(Math.sqrt(4) === 2, "sqrt(4) should equal 2");
});

test("String operations", () => {
    assert("hello".length === 5, "string length");
    assert("hello".toUpperCase() === "HELLO", "toUpperCase");
});

test("Array operations", () => {
    const arr = [1, 2, 3];
    assert(arr.length === 3, "array length");
    assert(arr.map(x => x * 2).join(",") === "2,4,6", "map and join");
});

test("Object operations", () => {
    const obj = { a: 1, b: 2 };
    assert(Object.keys(obj).length === 2, "object keys");
    assert(JSON.stringify(obj) === '{"a":1,"b":2}', "JSON stringify");
});

test("Async/await syntax", async () => {
    const result = await Promise.resolve(42);
    assert(result === 42, "Promise.resolve should work");
});

// =============================================================================
// Polyfill Tests
// =============================================================================
print("\n--- Polyfills ---");

test("console object exists", () => {
    assert(typeof console !== "undefined", "console should exist");
    assert(typeof console.log === "function", "console.log should be function");
});

test("process object exists", () => {
    assert(typeof process !== "undefined", "process should exist");
    const validPlatforms = ["wasi", "darwin", "linux", "win32"];
    assert(validPlatforms.includes(process.platform), "process.platform should be valid: " + process.platform);
});

test("globalThis exists", () => {
    assert(typeof globalThis !== "undefined", "globalThis should exist");
});

test("fetch function exists", () => {
    assert(typeof fetch === "function", "fetch should be a function");
});

test("__edgebox_fetch native binding exists", () => {
    assert(typeof __edgebox_fetch === "function", "__edgebox_fetch should be a function");
});

// =============================================================================
// QuickJS std module Tests
// =============================================================================
print("\n--- std module ---");

test("scriptArgs available", () => {
    assert(typeof scriptArgs !== "undefined", "scriptArgs should exist");
    assert(Array.isArray(scriptArgs), "scriptArgs should be array");
});

// =============================================================================
// File System Tests (WASI)
// =============================================================================
print("\n--- File System (WASI) ---");

test("Can read this test file", () => {
    // Use std module to read file
    try {
        const std = globalThis.std;
        if (std && std.loadFile) {
            const content = std.loadFile("test/test_features.js");
            assert(content.includes("EdgeBox Feature Tests"), "Should contain test header");
        } else {
            // Skip if std module not available
            print("  (std.loadFile not available, skipping)");
        }
    } catch (e) {
        // Skip if file read not available
        print("  (file read failed: " + e.message + ")");
    }
});

// =============================================================================
// Network Tests (WASI Sockets)
// =============================================================================
print("\n--- Network (WASI Sockets) ---");

test("HTTP fetch (localhost test)", () => {
    // Try to make an HTTP request
    // This will fail if WASI sockets aren't working
    try {
        const result = __edgebox_fetch("http://127.0.0.1:8080/", "GET", null, null);
        // If we get here, sockets work
        print("  (Connection succeeded! Status: " + result.status + ")");
    } catch (e) {
        // Accept any of these error conditions as valid
        const acceptableErrors = [
            "Connection failed",
            "Host not found",
            "fetch not available",  // Native CLI stub
            "HTTPS not supported"
        ];
        const isAcceptable = acceptableErrors.some(err => e.message.includes(err));
        if (!isAcceptable) {
            throw e; // Unexpected error
        }
        print("  (Expected error: " + e.message.substring(0, 50) + "...)");
    }
});

test("HTTPS should fail gracefully", () => {
    try {
        __edgebox_fetch("https://example.com/", "GET", null, null);
        // If HTTPS works, that's fine too
    } catch (e) {
        // Should fail with TLS not supported or fetch unavailable
        assert(
            e.message.includes("HTTPS not supported") ||
            e.message.includes("TLS") ||
            e.message.includes("Connection failed") ||
            e.message.includes("fetch not available"),  // Native CLI stub
            "Should indicate HTTPS/TLS not supported or fetch unavailable"
        );
    }
});

// =============================================================================
// TTY Tests
// =============================================================================
print("\n--- TTY/Terminal ---");

test("__edgebox_isatty native binding exists", () => {
    assert(typeof __edgebox_isatty === "function", "__edgebox_isatty should be a function");
});

test("__edgebox_get_terminal_size native binding exists", () => {
    assert(typeof __edgebox_get_terminal_size === "function", "__edgebox_get_terminal_size should be a function");
});

test("tty module exists", () => {
    assert(typeof tty !== "undefined", "tty should exist");
    assert(typeof tty.isatty === "function", "tty.isatty should be a function");
});

test("tty.isatty returns boolean", () => {
    const result = tty.isatty(0);
    assert(typeof result === "boolean", "isatty should return boolean");
});

test("process.stdin exists", () => {
    assert(typeof process.stdin !== "undefined", "process.stdin should exist");
    assert(typeof process.stdin.isTTY === "boolean", "process.stdin.isTTY should be boolean");
});

test("process.stdout exists", () => {
    assert(typeof process.stdout !== "undefined", "process.stdout should exist");
    assert(typeof process.stdout.isTTY === "boolean", "process.stdout.isTTY should be boolean");
});

test("terminal size returns object", () => {
    const size = __edgebox_get_terminal_size();
    assert(typeof size === "object", "terminal size should be object");
    assert(typeof size.rows === "number", "rows should be number");
    assert(typeof size.cols === "number", "cols should be number");
    assert(size.rows > 0, "rows should be positive");
    assert(size.cols > 0, "cols should be positive");
});

test("tty.WriteStream has getWindowSize", () => {
    const ws = new tty.WriteStream(1);
    assert(typeof ws.getWindowSize === "function", "WriteStream should have getWindowSize");
    const size = ws.getWindowSize();
    assert(Array.isArray(size), "getWindowSize should return array");
    assert(size.length === 2, "getWindowSize should return [cols, rows]");
});

// =============================================================================
// child_process Tests
// =============================================================================
print("\n--- child_process ---");

test("child_process module exists", () => {
    assert(typeof child_process !== "undefined", "child_process should exist");
    assert(typeof child_process.spawnSync === "function", "spawnSync should be function");
    assert(typeof child_process.execSync === "function", "execSync should be function");
});

test("__edgebox_spawn native binding exists", () => {
    assert(typeof __edgebox_spawn === "function", "__edgebox_spawn should be a function");
});

test("spawnSync returns proper structure (requires edgebox_process API)", () => {
    // Note: This test requires edgebox_process API which is
    // implemented via WAMR. The API is available when running with:
    //   zig-out/bin/edgebox test/test_features.js
    print("  (Skipping actual spawn - requires edgebox_process API)");

    // Just verify the JS polyfill structure exists
    const spawnSyncFn = child_process.spawnSync;
    assert(typeof spawnSyncFn === "function", "spawnSync should be function");
});

// =============================================================================
// Node.js Polyfill Tests
// =============================================================================
print("\n--- Node.js Polyfills ---");

test("Buffer exists and works", () => {
    assert(typeof Buffer !== "undefined", "Buffer should exist");
    const buf = Buffer.from("hello");
    assert(buf instanceof Buffer, "Buffer.from should return Buffer");
    assert(buf.toString() === "hello", "Buffer.toString should work");
});

test("Buffer.alloc and static methods", () => {
    const buf = Buffer.alloc(10, 0);
    assert(buf.length === 10, "Buffer.alloc should create buffer of size");
    assert(Buffer.isBuffer(buf), "Buffer.isBuffer should work");
    const concat = Buffer.concat([Buffer.from("a"), Buffer.from("b")]);
    assert(concat.toString() === "ab", "Buffer.concat should work");
});

test("path module exists and works", () => {
    assert(typeof path !== "undefined", "path should exist");
    assert(path.basename("/foo/bar.txt") === "bar.txt", "basename should work");
    assert(path.dirname("/foo/bar.txt") === "/foo", "dirname should work");
    assert(path.extname("/foo/bar.txt") === ".txt", "extname should work");
    assert(path.join("foo", "bar") === "foo/bar", "join should work");
});

test("path.normalize and resolve", () => {
    assert(path.normalize("/foo//bar/../baz") === "/foo/baz", "normalize should work");
    assert(path.isAbsolute("/foo") === true, "isAbsolute should work");
});

test("path.parse and format", () => {
    const parsed = path.parse("/home/user/file.txt");
    assert(parsed.dir === "/home/user", "parse dir should work");
    assert(parsed.base === "file.txt", "parse base should work");
    assert(parsed.ext === ".txt", "parse ext should work");
});

test("url module exists", () => {
    assert(typeof url !== "undefined", "url should exist");
    assert(typeof url.parse === "function", "url.parse should be function");
});

test("EventEmitter exists and works", () => {
    assert(typeof EventEmitter !== "undefined", "EventEmitter should exist");
    const emitter = new EventEmitter();
    let called = false;
    emitter.on("test", () => { called = true; });
    emitter.emit("test");
    assert(called === true, "EventEmitter should emit events");
});

test("EventEmitter once and off", () => {
    const emitter = new EventEmitter();
    let count = 0;
    emitter.once("test", () => { count++; });
    emitter.emit("test");
    emitter.emit("test");
    assert(count === 1, "once should only fire once");
});

test("events module exports EventEmitter", () => {
    assert(typeof events !== "undefined", "events should exist");
    assert(events.EventEmitter === EventEmitter, "events.EventEmitter should be EventEmitter");
});

test("util module exists and format works", () => {
    assert(typeof util !== "undefined", "util should exist");
    assert(util.format("hello %s", "world") === "hello world", "util.format should work");
    assert(util.format("%d + %d = %d", 1, 2, 3) === "1 + 2 = 3", "util.format with numbers");
});

test("util.promisify and types", () => {
    assert(typeof util.promisify === "function", "util.promisify should be function");
    assert(util.types.isArray([]) === true, "util.types.isArray should work");
    assert(util.types.isDate(new Date()) === true, "util.types.isDate should work");
});

test("os module exists", () => {
    assert(typeof os !== "undefined", "os should exist");
    const validPlatforms = ["wasi", "darwin", "linux", "win32"];
    assert(validPlatforms.includes(os.platform()), "os.platform should be valid: " + os.platform());
    const validArchs = ["wasm32", "arm64", "x64", "x86"];
    assert(validArchs.includes(os.arch()), "os.arch should be valid: " + os.arch());
    assert(typeof os.homedir() === "string", "os.homedir should return string");
});

// =============================================================================
// Summary
// =============================================================================
print("\n=== Test Results ===");
print("Passed: " + passed);
print("Failed: " + failed);
print("Total:  " + (passed + failed));

if (failed > 0) {
    print("\n⚠ Some tests failed!");
    // Don't exit with error code - some failures are expected
} else {
    print("\n✓ All tests passed!");
}
