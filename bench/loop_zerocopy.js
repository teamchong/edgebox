// Zero-Copy WASM Memory Benchmark
// Demonstrates using __wasmArray() to allocate data directly in WASM memory.
// When the trampoline detects arr.buffer === __wbuf, it skips copy entirely.

function sumArray(arr) {
    var acc = 0;
    for (var i = 0; i < arr.length; i++) {
        acc = acc + arr[i];
    }
    return acc;
}

var SIZE = 100000;
var RUNS = 1000;
var EXPECTED = 4950000;
var log = typeof print === "function" ? print : console.log;

// Use __wasmArray if available (worker path), else regular array
var data;
if (typeof __wasmArray === 'function') {
    data = __wasmArray(Int32Array, SIZE);
    for (var i = 0; i < SIZE; i++) data[i] = i % 100;
} else {
    data = new Int32Array(SIZE);
    for (var i = 0; i < SIZE; i++) data[i] = i % 100;
}

var start = performance.now();
var result;
for (var i = 0; i < RUNS; i++) {
    result = sumArray(data);
}
var elapsed = performance.now() - start;

if (result !== EXPECTED) {
    log("FAIL: got " + result + ", expected " + EXPECTED);
} else {
    log("loop_zerocopy: " + Math.round(elapsed) + "ms (" + RUNS + " runs, result=" + EXPECTED + ")");
}
