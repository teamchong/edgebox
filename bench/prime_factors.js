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
var EXPECTED = 168530;  // Pre-computed expected value

var times = [];
var result;

for (var i = 0; i < RUNS; i++) {
    var start = performance.now();
    result = countAllFactors(LIMIT);
    times.push(performance.now() - start);
}

var total = 0;
for (var i = 0; i < times.length; i++) total += times[i];
var avg = total / times.length;

if (result !== EXPECTED) {
    log("FAIL: prime_factors = " + result + ", expected " + EXPECTED);
} else {
    log(EXPECTED + " (" + avg.toFixed(2) + "ms avg, " + times.map(function(t) { return t.toFixed(0); }).join("/") + ")");
}
