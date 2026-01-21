// Loop benchmark - array sum with regular JS array
// Tests: frozen array iteration performance (NOT TypedArray)

function sumArray(arr) {
    var acc = 0;
    for (var i = 0; i < arr.length; i++) {
        acc = acc + arr[i];
    }
    return acc;
}

var SIZE = 100000;
var RUNS = 1000;  // 1k runs
// sum of (0..99) repeated 1000 times = 4950 * 1000 = 4950000
var EXPECTED = 4950000;
var log = typeof print === "function" ? print : console.log;

// Use regular JS array (NOT TypedArray)
var data = [];
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
    log(EXPECTED + " (" + elapsed.toFixed(1) + "ms total, " + avg.toFixed(3) + "ms avg)");
}
