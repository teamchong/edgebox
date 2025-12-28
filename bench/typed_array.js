// TypedArray benchmark - tests Int32Array access performance
// Measures overhead of current JSValue-based access vs direct buffer access

function sumInt32Array(arr) {
    var sum = 0;
    for (var i = 0; i < arr.length; i++) {
        sum = sum + arr[i];
    }
    return sum;
}

var SIZE = 100000;
var RUNS = 10000;  // 10k runs for ~1+ seconds total runtime
var log = typeof print === "function" ? print : console.log;

// Create Int32Array with small values to avoid overflow
var data = new Int32Array(SIZE);
for (var i = 0; i < SIZE; i++) {
    data[i] = i % 100;  // Values 0-99 to avoid overflow
}

// Expected: sum of (0..99) repeated 1000 times = 4950 * 1000 = 4950000
var EXPECTED = 4950000;

// Measure total time for all runs
var start = performance.now();
var result;
for (var j = 0; j < RUNS; j++) {
    result = sumInt32Array(data);
}
var elapsed = performance.now() - start;
var avg = elapsed / RUNS;

if (result !== EXPECTED) {
    log("FAIL: got " + result + ", expected " + EXPECTED);
} else {
    // Total time first for benchmark extraction
    log("Int32Array sum (" + SIZE + " elements) = " + result + " (" + elapsed.toFixed(1) + "ms total, " + avg.toFixed(3) + "ms avg)");
}
