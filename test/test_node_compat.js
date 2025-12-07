// Node.js Compatibility Tests for EdgeBox
// Tests all APIs needed for Claude Code and common Node.js apps

let passed = 0;
let failed = 0;
let skipped = 0;

function test(name, fn) {
    try {
        fn();
        print(`✓ ${name}`);
        passed++;
    } catch (e) {
        print(`✗ ${name}: ${e.message}`);
        failed++;
    }
}

function skip(name, reason) {
    print(`○ ${name} (${reason})`);
    skipped++;
}

function assert(condition, msg) {
    if (!condition) throw new Error(msg || "Assertion failed");
}

print("=== Node.js Compatibility Tests ===\n");

// ============================================
// GLOBAL OBJECTS
// ============================================
print("--- Global Objects ---");

test("globalThis exists", () => {
    assert(typeof globalThis === "object");
});

test("process exists", () => {
    assert(typeof process === "object");
});

test("process.env exists", () => {
    assert(typeof process.env === "object");
});

test("process.argv exists", () => {
    assert(Array.isArray(process.argv) || Array.isArray(scriptArgs));
});

test("process.cwd() exists", () => {
    assert(typeof process.cwd === "function");
});

test("process.exit exists", () => {
    assert(typeof process.exit === "function");
});

test("process.platform exists", () => {
    assert(typeof process.platform === "string");
});

test("console exists", () => {
    assert(typeof console === "object");
    assert(typeof console.log === "function");
});

// ============================================
// TIMERS
// ============================================
print("\n--- Timers ---");

test("setTimeout exists", () => {
    assert(typeof setTimeout === "function", "setTimeout not found");
});

test("setInterval exists", () => {
    assert(typeof setInterval === "function", "setInterval not found");
});

test("clearTimeout exists", () => {
    assert(typeof clearTimeout === "function", "clearTimeout not found");
});

test("clearInterval exists", () => {
    assert(typeof clearInterval === "function", "clearInterval not found");
});

test("setImmediate exists", () => {
    assert(typeof setImmediate === "function", "setImmediate not found");
});

test("queueMicrotask exists", () => {
    assert(typeof queueMicrotask === "function", "queueMicrotask not found");
});

// ============================================
// TEXT ENCODING
// ============================================
print("\n--- Text Encoding ---");

test("TextEncoder exists", () => {
    assert(typeof TextEncoder === "function", "TextEncoder not found");
});

test("TextDecoder exists", () => {
    assert(typeof TextDecoder === "function", "TextDecoder not found");
});

test("TextEncoder encodes correctly", () => {
    const encoder = new TextEncoder();
    const encoded = encoder.encode("hello");
    assert(encoded instanceof Uint8Array);
    assert(encoded.length === 5);
});

test("TextDecoder decodes correctly", () => {
    const decoder = new TextDecoder();
    const decoded = decoder.decode(new Uint8Array([104, 101, 108, 108, 111]));
    assert(decoded === "hello");
});

// ============================================
// URL
// ============================================
print("\n--- URL ---");

test("URL exists", () => {
    assert(typeof URL === "function", "URL not found");
});

test("URLSearchParams exists", () => {
    assert(typeof URLSearchParams === "function", "URLSearchParams not found");
});

test("URL parsing works", () => {
    const url = new URL("https://example.com:8080/path?query=1#hash");
    assert(url.hostname === "example.com");
    assert(url.port === "8080");
    assert(url.pathname === "/path");
    assert(url.search === "?query=1");
    assert(url.hash === "#hash");
});

// ============================================
// ABORT CONTROLLER
// ============================================
print("\n--- AbortController ---");

test("AbortController exists", () => {
    assert(typeof AbortController === "function", "AbortController not found");
});

test("AbortSignal exists", () => {
    assert(typeof AbortSignal === "function", "AbortSignal not found");
});

test("AbortController.abort() works", () => {
    const controller = new AbortController();
    assert(controller.signal.aborted === false);
    controller.abort();
    assert(controller.signal.aborted === true);
});

// ============================================
// BUFFER
// ============================================
print("\n--- Buffer ---");

test("Buffer exists", () => {
    assert(typeof Buffer === "function");
});

test("Buffer.from works", () => {
    const buf = Buffer.from("hello");
    assert(buf.length === 5);
});

test("Buffer.alloc works", () => {
    const buf = Buffer.alloc(10);
    assert(buf.length === 10);
});

test("Buffer.toString works", () => {
    const buf = Buffer.from("hello");
    assert(buf.toString() === "hello");
});

test("Buffer.concat works", () => {
    const buf = Buffer.concat([Buffer.from("hel"), Buffer.from("lo")]);
    assert(buf.toString() === "hello");
});

// ============================================
// CRYPTO
// ============================================
print("\n--- Crypto ---");

test("crypto exists", () => {
    assert(typeof crypto === "object" || typeof globalThis.crypto === "object", "crypto not found");
});

test("crypto.randomUUID exists", () => {
    const c = crypto || globalThis.crypto;
    assert(typeof c?.randomUUID === "function", "crypto.randomUUID not found");
});

test("crypto.getRandomValues exists", () => {
    const c = crypto || globalThis.crypto;
    assert(typeof c?.getRandomValues === "function", "crypto.getRandomValues not found");
});

// ============================================
// REQUIRE / MODULE SYSTEM
// ============================================
print("\n--- Module System ---");

test("require exists", () => {
    assert(typeof require === "function", "require not found");
});

test("require('fs') works", () => {
    const fs = require("fs");
    assert(typeof fs === "object", "fs module not found");
});

test("require('path') works", () => {
    const path = require("path");
    assert(typeof path === "object", "path module not found");
    assert(typeof path.join === "function");
});

test("require('os') works", () => {
    const os = require("os");
    assert(typeof os === "object", "os module not found");
});

test("require('events') works", () => {
    const events = require("events");
    assert(typeof events === "object", "events module not found");
});

test("require('util') works", () => {
    const util = require("util");
    assert(typeof util === "object", "util module not found");
});

test("require('crypto') works", () => {
    const crypto = require("crypto");
    assert(typeof crypto === "object", "crypto module not found");
});

test("require('http') works", () => {
    const http = require("http");
    assert(typeof http === "object", "http module not found");
});

test("require('https') works", () => {
    const https = require("https");
    assert(typeof https === "object", "https module not found");
});

test("require('stream') works", () => {
    const stream = require("stream");
    assert(typeof stream === "object", "stream module not found");
});

test("require('child_process') works", () => {
    const cp = require("child_process");
    assert(typeof cp === "object", "child_process module not found");
});

test("require('net') works", () => {
    const net = require("net");
    assert(typeof net === "object", "net module not found");
});

test("require('dns') works", () => {
    const dns = require("dns");
    assert(typeof dns === "object", "dns module not found");
});

// ============================================
// FS MODULE
// ============================================
print("\n--- fs Module ---");

test("fs.existsSync exists", () => {
    const fs = require("fs");
    assert(typeof fs.existsSync === "function", "fs.existsSync not found");
});

test("fs.readFileSync exists", () => {
    const fs = require("fs");
    assert(typeof fs.readFileSync === "function", "fs.readFileSync not found");
});

test("fs.writeFileSync exists", () => {
    const fs = require("fs");
    assert(typeof fs.writeFileSync === "function", "fs.writeFileSync not found");
});

test("fs.readdirSync exists", () => {
    const fs = require("fs");
    assert(typeof fs.readdirSync === "function", "fs.readdirSync not found");
});

test("fs.statSync exists", () => {
    const fs = require("fs");
    assert(typeof fs.statSync === "function", "fs.statSync not found");
});

test("fs.mkdirSync exists", () => {
    const fs = require("fs");
    assert(typeof fs.mkdirSync === "function", "fs.mkdirSync not found");
});

test("fs.unlinkSync exists", () => {
    const fs = require("fs");
    assert(typeof fs.unlinkSync === "function", "fs.unlinkSync not found");
});

test("fs.promises exists", () => {
    const fs = require("fs");
    assert(typeof fs.promises === "object", "fs.promises not found");
});

// ============================================
// ASYNC PRIMITIVES
// ============================================
print("\n--- Async Primitives ---");

test("Promise exists", () => {
    assert(typeof Promise === "function");
});

test("async/await works", () => {
    const fn = async () => 42;
    assert(typeof fn === "function");
});

// ============================================
// STREAMS
// ============================================
print("\n--- Streams ---");

test("process.stdin exists", () => {
    assert(typeof process.stdin === "object", "process.stdin not found");
});

test("process.stdout exists", () => {
    assert(typeof process.stdout === "object", "process.stdout not found");
});

test("process.stderr exists", () => {
    assert(typeof process.stderr === "object", "process.stderr not found");
});

// ============================================
// SUMMARY
// ============================================
print("\n=== Summary ===");
print(`Passed: ${passed}`);
print(`Failed: ${failed}`);
print(`Skipped: ${skipped}`);
print(`Total: ${passed + failed + skipped}`);

if (failed > 0) {
    print("\n⚠️  Some tests failed - these APIs need to be implemented");
}
