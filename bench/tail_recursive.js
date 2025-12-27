// Tail recursive benchmark - sum(1..N)
// Tests: function call overhead (frozen recursive functions)

function sumTailRec(n, acc) {
    if (n <= 0) return acc;
    return sumTailRec(n - 1, acc + n);
}

var N = 10000;
var RUNS = 10000;
var EXPECTED = N * (N + 1) / 2;  // sum(1..10000) = 50005000
var log = typeof print === "function" ? print : console.log;

// Measure total time for all runs
var start = performance.now();
var result;
for (var i = 0; i < RUNS; i++) {
    result = sumTailRec(N, 0);
}
var elapsed = performance.now() - start;
var avg = elapsed / RUNS;

if (result !== EXPECTED) {
    log("FAIL: got " + result + ", expected " + EXPECTED);
} else {
    log(EXPECTED + " (" + avg.toFixed(3) + "ms avg, " + elapsed.toFixed(1) + "ms total)");
}
