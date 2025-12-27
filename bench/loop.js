// Loop benchmark - array sum
// Tests: frozen array iteration (get_array_el, get_length opcodes)

function sumArray(arr) {
    var acc = 0;
    for (var i = 0; i < arr.length; i++) {
        acc = acc + arr[i];
    }
    return acc;
}

var SIZE = 100000;
var RUNS = 1000;
var EXPECTED = (SIZE - 1) * SIZE / 2;  // sum(0..99999) = 4999950000
var log = typeof print === "function" ? print : console.log;

// Build array once
var data = [];
for (var i = 0; i < SIZE; i++) data.push(i);

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
    log(EXPECTED + " (" + avg.toFixed(3) + "ms avg, " + elapsed.toFixed(1) + "ms total)");
}
