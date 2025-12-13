// Tail-recursive sum benchmark - simple numeric recursion
// Computes sum(1..N) = N*(N+1)/2

// Use iterative approach since tail-recursive hangs in EdgeBox
function sum(n) {
    var total = 0;
    for (var i = 1; i <= n; i++) total += i;
    return total;
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
