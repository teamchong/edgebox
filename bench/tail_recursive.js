// Tail recursive benchmark - sum(1..N)
// Tests: function call overhead (frozen recursive functions)

function sumTailRec(n, acc) {
    if (n <= 0) return acc;
    return sumTailRec(n - 1, acc + n);
}

var N = 10000;
var RUNS = 1000000;  // 1M runs for ~150+ ms total runtime
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
    // Total time first for benchmark extraction
    log(EXPECTED + " (" + elapsed.toFixed(1) + "ms total, " + avg.toFixed(3) + "ms avg)");
}
