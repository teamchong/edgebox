// TypedArray benchmark - tests zero-allocation Int32Array access
// Measures overhead of current JSValue-based access vs direct buffer access
// Tests: Int32Array sum with frozen code

function sumInt32Array(arr) {
    var sum = 0;
    for (var i = 0; i < arr.length; i++) {
        sum = sum + arr[i];
    }
    return sum;
}

var SIZE = 10000;
var RUNS = 100;
var log = typeof print === "function" ? print : console.log;

// Create Int32Array
var data = new Int32Array(SIZE);
for (var i = 0; i < SIZE; i++) {
    data[i] = i;
}

var EXPECTED = (SIZE - 1) * SIZE / 2;  // sum(0..9999) = 49995000

var times = [];
for (var j = 0; j < RUNS; j++) {
    var start = performance.now();
    var result = sumInt32Array(data);
    times.push(performance.now() - start);
}

// Calculate avg without reduce
var total = 0;
for (var k = 0; k < times.length; k++) {
    total = total + times[k];
}
var avg = total / times.length;

if (result !== EXPECTED) {
    log("FAIL: got " + result + ", expected " + EXPECTED);
} else {
    log("Int32Array sum (" + SIZE + " elements) = " + result + " (" + avg.toFixed(2) + "ms avg, " + RUNS + " iterations)");
}
