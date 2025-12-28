// Loop benchmark - array sum
// Tests: frozen array iteration with TypedArray fast path

function sumArray(arr) {
    var acc = 0;
    for (var i = 0; i < arr.length; i++) {
        acc = acc + arr[i];
    }
    return acc;
}

var SIZE = 100000;
var RUNS = 10000;  // 10k runs for ~1+ seconds total runtime
// sum of (0..99) repeated 1000 times = 4950 * 1000 = 4950000
var EXPECTED = 4950000;
var log = typeof print === "function" ? print : console.log;

// Use Int32Array for fast path (direct buffer access)
// Use i % 100 to keep values small and avoid int32 overflow in sum
var data = new Int32Array(SIZE);
for (var i = 0; i < SIZE; i++) data[i] = i % 100;

// Measure total time for all runs
var start = performance.now();
var result;
for (var i = 0; i < RUNS; i++) {
    result = sumArray(data);
}
var elapsed = performance.now() - start;
var avg = elapsed / RUNS;

if (result !== EXPECTED) {
    log("FAIL: got " + result + ", expected " + EXPECTED);
} else {
    // Total time first for benchmark extraction
    log(EXPECTED + " (" + elapsed.toFixed(1) + "ms total, " + avg.toFixed(3) + "ms avg)");
}
