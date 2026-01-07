// JSON Pipeline Benchmark
// Tests: JSON.parse → mutate → JSON.stringify
// Common serverless/API workload

var log = typeof print === "function" ? print : console.log;

// Pre-generated payloads (avoid Math.random in runtime)
var SMALL = '{"id":1,"name":"test","value":123.45,"active":true,"tags":["a","b"]}';

// Build medium payload (~1KB)
var items = [];
for (var i = 0; i < 10; i++) {
    items.push('{"id":' + i + ',"name":"item' + i + '","value":' + (i * 1.5) + ',"tags":["x","y"]}');
}
var MEDIUM = '{"items":[' + items.join(',') + '],"total":10,"page":1}';

function jsonRoundtrip(payload) {
    var obj = JSON.parse(payload);
    obj.processed = true;
    return JSON.stringify(obj);
}

var RUNS_SMALL = 5000;
var RUNS_MEDIUM = 2000;
var result, start, elapsed;

log("=== JSON Pipeline Benchmark ===");
log("Small payload: " + SMALL.length + " bytes");
log("Medium payload: " + MEDIUM.length + " bytes");
log("");

// Small payload
start = performance.now();
for (var i = 0; i < RUNS_SMALL; i++) {
    result = jsonRoundtrip(SMALL);
}
elapsed = performance.now() - start;
log("JSON small (" + SMALL.length + "B): " + Math.floor(RUNS_SMALL / (elapsed / 1000)) + " ops/sec (" + elapsed.toFixed(1) + "ms total, " + (elapsed / RUNS_SMALL).toFixed(3) + "ms avg)");

// Medium payload
start = performance.now();
for (var i = 0; i < RUNS_MEDIUM; i++) {
    result = jsonRoundtrip(MEDIUM);
}
elapsed = performance.now() - start;
log("JSON medium (" + MEDIUM.length + "B): " + Math.floor(RUNS_MEDIUM / (elapsed / 1000)) + " ops/sec (" + elapsed.toFixed(1) + "ms total, " + (elapsed / RUNS_MEDIUM).toFixed(3) + "ms avg)");
