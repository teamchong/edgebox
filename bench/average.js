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
// Sum of 0..999999 = 499999500000, average = 499999.5
var EXPECTED = 499999.5;

// Create array once
var data = new Array(SIZE);
for (var i = 0; i < SIZE; i++) {
    data[i] = i;
}

var times = [];
var result;

for (var i = 0; i < RUNS; i++) {
    var start = performance.now();
    result = average(data);
    times.push(performance.now() - start);
}

var total = 0;
for (var i = 0; i < times.length; i++) total += times[i];
var avg = total / times.length;

if (result !== EXPECTED) {
    log("FAIL: average = " + result + ", expected " + EXPECTED);
} else {
    log(EXPECTED + " (" + avg.toFixed(2) + "ms avg, " + times.map(function(t) { return t.toFixed(1); }).join("/") + ")");
}
