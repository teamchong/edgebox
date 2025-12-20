// Const closure benchmark - tests SINT mode with read-only closure vars
// Classic recursive loop with const bound: loop(i, N) where N is captured
// Tests: 1-arg self-recursive function with 1 const closure var

const N = 10000;

function loop(i) {
    if (i >= N) return i;
    return loop(i + 1);
}

var RUNS = 1000;
var log = typeof print === "function" ? print : console.log;

var times = [];
for (var j = 0; j < RUNS; j++) {
    var start = performance.now();
    var result = loop(0);
    times.push(performance.now() - start);
}

// Calculate avg without reduce
var total = 0;
for (var k = 0; k < times.length; k++) {
    total = total + times[k];
}
var avg = total / times.length;

if (result !== N) {
    log("FAIL: got " + result + ", expected " + N);
} else {
    log("loop(0) to " + N + " = " + result + " (" + avg.toFixed(2) + "ms avg, " + RUNS + " iterations)");
}
