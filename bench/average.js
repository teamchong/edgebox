// Average Benchmark
// Tests: simple array iteration, accumulation, division
// Computes average of 1M numbers

var log = typeof print === "function" ? print : console.log;

function average(arr) {
    var sum = 0;
    for (var i = 0; i < arr.length; i++) {
        sum += arr[i];
    }
    return sum / arr.length;
}

var SIZE = 1000000;
var RUNS = 1000;
var EXPECTED = 499999.5;

// Create array once
var data = new Array(SIZE);
for (var i = 0; i < SIZE; i++) {
    data[i] = i;
}

// Measure total time for ALL runs (only 2 performance.now calls)
var result;
var start = performance.now();
for (var i = 0; i < RUNS; i++) {
    result = average(data);
}
var elapsed = performance.now() - start;

if (result !== EXPECTED) {
    log("FAIL: average = " + result + ", expected " + EXPECTED);
} else {
    log(EXPECTED + " (" + elapsed.toFixed(1) + "ms total, " + (elapsed / RUNS).toFixed(2) + "ms avg)");
}
