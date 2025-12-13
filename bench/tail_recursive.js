// Tail-recursive sum benchmark - actual tail recursion
// Computes sum(1..N) using tail call optimization pattern
//
// Tail recursion: the recursive call is the LAST operation,
// allowing the runtime to reuse the stack frame.

function sumTailRec(n, acc) {
    if (n <= 0) return acc;
    return sumTailRec(n - 1, acc + n);  // tail call
}

// Wrapper for clean API
function sum(n) {
    return sumTailRec(n, 0);
}

var N = 1000;
var EXPECTED = N * (N + 1) / 2;  // sum(1..1000) = 500500
var RUNS = 100;
var total_time = 0;
var log = typeof print === 'function' ? print : console.log;

for (var i = 0; i < RUNS; i++) {
    var start = performance.now();
    var result = sum(N);
    var elapsed = performance.now() - start;
    total_time += elapsed;
    if (result !== EXPECTED) {
        log("FAIL: sum = " + result + ", expected " + EXPECTED);
    }
}

var avg = total_time / RUNS;
log(EXPECTED + " (" + avg.toFixed(2) + "ms avg)");
