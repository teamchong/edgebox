// Buffer Operations Benchmark
// Tests: slice, subarray, copy, concat

var log = typeof print === "function" ? print : console.log;

// Create test buffers
var buf4k = Buffer.alloc(4 * 1024);
var buf64k = Buffer.alloc(64 * 1024);
for (var i = 0; i < buf4k.length; i++) buf4k[i] = i & 0xff;
for (var i = 0; i < buf64k.length; i++) buf64k[i] = i & 0xff;

var RUNS = 5000;
var result, start, elapsed;

log("=== Buffer Operations Benchmark ===");
log("");

// Slice (creates copy)
log("--- slice (copy) ---");
start = performance.now();
for (var i = 0; i < RUNS; i++) {
    result = buf4k.slice(0, 2048);
}
elapsed = performance.now() - start;
log("slice 4KB: " + Math.floor(RUNS / (elapsed / 1000)) + " ops/sec (" + elapsed.toFixed(1) + "ms)");

RUNS = 500;
start = performance.now();
for (var i = 0; i < RUNS; i++) {
    result = buf64k.slice(0, 32768);
}
elapsed = performance.now() - start;
log("slice 64KB: " + Math.floor(RUNS / (elapsed / 1000)) + " ops/sec (" + elapsed.toFixed(1) + "ms)");

// Subarray (view, no copy)
log("");
log("--- subarray (view) ---");
RUNS = 50000;
start = performance.now();
for (var i = 0; i < RUNS; i++) {
    result = buf64k.subarray(0, 32768);
}
elapsed = performance.now() - start;
log("subarray 64KB: " + Math.floor(RUNS / (elapsed / 1000)) + " ops/sec (" + elapsed.toFixed(1) + "ms)");

// Copy
log("");
log("--- copy ---");
var target = Buffer.alloc(64 * 1024);
RUNS = 500;
start = performance.now();
for (var i = 0; i < RUNS; i++) {
    buf64k.copy(target);
}
elapsed = performance.now() - start;
log("copy 64KB: " + Math.floor(RUNS / (elapsed / 1000)) + " ops/sec (" + elapsed.toFixed(1) + "ms)");

// Concat
log("");
log("--- concat ---");
var chunks = [buf4k, buf4k, buf4k, buf4k]; // 16KB total
RUNS = 1000;
start = performance.now();
for (var i = 0; i < RUNS; i++) {
    result = Buffer.concat(chunks);
}
elapsed = performance.now() - start;
log("concat 4x4KB: " + Math.floor(RUNS / (elapsed / 1000)) + " ops/sec (" + elapsed.toFixed(1) + "ms)");
