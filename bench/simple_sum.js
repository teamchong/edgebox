// Simple sum benchmark - iterative numeric loop
// Computes sum(1..N) = N*(N+1)/2
function sum(n) {
    var total = 0;
    for (var i = 1; i <= n; i++) total += i;
    return total;
}

var N = 1000;
var EXPECTED = N * (N + 1) / 2;  // sum(1..1000) = 500500
var RUNS = 100000;  // 100k runs
var log = typeof print === 'function' ? print : console.log;

// Measure total time for all iterations (not per-iteration overhead)
var start = performance.now();
var result;
for (var i = 0; i < RUNS; i++) {
    result = sum(N);
}
var elapsed = performance.now() - start;

if (result !== EXPECTED) {
    log("FAIL: sum = " + result + ", expected " + EXPECTED);
}

var avg = elapsed / RUNS;
log(EXPECTED + " (" + avg.toFixed(4) + "ms avg)");
