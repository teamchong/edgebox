// UTF-8 Codec Benchmark
// Tests: TextEncoder.encode() and TextDecoder.decode()

var log = typeof print === "function" ? print : console.log;

// Generate ASCII string (simple)
var ascii1k = "";
for (var i = 0; i < 1024; i++) {
    ascii1k = ascii1k + String.fromCharCode(65 + (i % 26)); // A-Z
}

var encoder = new TextEncoder();
var decoder = new TextDecoder();

// Pre-encode for decode tests
var asciiBytes = encoder.encode(ascii1k);

var RUNS = 2000;
var result, start, elapsed;

log("=== UTF-8 Codec Benchmark ===");
log("String size: 1KB");
log("");

// ASCII encode
log("--- Encode ---");
start = performance.now();
for (var i = 0; i < RUNS; i++) {
    result = encoder.encode(ascii1k);
}
elapsed = performance.now() - start;
log("Encode 1KB: " + Math.floor(RUNS / (elapsed / 1000)) + " ops/sec (" + elapsed.toFixed(1) + "ms)");

// ASCII decode
log("");
log("--- Decode ---");
start = performance.now();
for (var i = 0; i < RUNS; i++) {
    result = decoder.decode(asciiBytes);
}
elapsed = performance.now() - start;
log("Decode 1KB: " + Math.floor(RUNS / (elapsed / 1000)) + " ops/sec (" + elapsed.toFixed(1) + "ms)");
