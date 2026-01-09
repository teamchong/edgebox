// Prime Factors Benchmark
// Tests: integer division, modulo, loops, array push
// Factors numbers from 2 to N and counts total factors

var log = typeof print === "function" ? print : console.log;

function primeFactors(n) {
    var factors = [];
    var d = 2;
    while (d * d <= n) {
        while (n % d === 0) {
            factors.push(d);
            n = Math.floor(n / d);
        }
        d++;
    }
    if (n > 1) {
        factors.push(n);
    }
    return factors;
}

function countAllFactors(limit) {
    var total = 0;
    for (var i = 2; i <= limit; i++) {
        total += primeFactors(i).length;
    }
    return total;
}

var LIMIT = 50000;
var RUNS = 200;
var EXPECTED = 168530;

// Measure total time for ALL runs (only 2 performance.now calls)
var result;
var start = performance.now();
for (var i = 0; i < RUNS; i++) {
    result = countAllFactors(LIMIT);
}
var elapsed = performance.now() - start;

if (result !== EXPECTED) {
    log("FAIL: prime_factors = " + result + ", expected " + EXPECTED);
} else {
    log(EXPECTED + " (" + elapsed.toFixed(1) + "ms total, " + (elapsed / RUNS).toFixed(2) + "ms avg)");
}
