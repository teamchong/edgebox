// Base64 Benchmark
// Tests: Buffer.toString('base64') and Buffer.from(b64, 'base64')

var log = typeof print === "function" ? print : console.log;

// Create buffer with deterministic data
function createBuffer(size) {
    var buf = Buffer.alloc(size);
    for (var i = 0; i < size; i++) {
        buf[i] = i & 0xff;
    }
    return buf;
}

var buf1k = createBuffer(1024);
var b64_1k = buf1k.toString("base64");

var RUNS = 2000;
var result, start, elapsed;

log("=== Base64 Benchmark ===");
log("Buffer size: 1KB");
log("");

// Roundtrip
start = performance.now();
for (var i = 0; i < RUNS; i++) {
    var encoded = buf1k.toString("base64");
    result = Buffer.from(encoded, "base64");
}
elapsed = performance.now() - start;
log("Base64 1KB roundtrip: " + Math.floor(RUNS / (elapsed / 1000)) + " ops/sec (" + elapsed.toFixed(1) + "ms)");

// Encode only
start = performance.now();
for (var i = 0; i < RUNS; i++) {
    result = buf1k.toString("base64");
}
elapsed = performance.now() - start;
log("Base64 1KB encode: " + Math.floor(RUNS / (elapsed / 1000)) + " ops/sec (" + elapsed.toFixed(1) + "ms)");

// Decode only
start = performance.now();
for (var i = 0; i < RUNS; i++) {
    result = Buffer.from(b64_1k, "base64");
}
elapsed = performance.now() - start;
log("Base64 1KB decode: " + Math.floor(RUNS / (elapsed / 1000)) + " ops/sec (" + elapsed.toFixed(1) + "ms)");
