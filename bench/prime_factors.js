// Prime Factors Benchmark
// Original from https://github.com/vExcess/js-engine-bench (9450 numbers)
// Scaled down to 2000 for EdgeBox

var log = typeof print === "function" ? print : console.log;

function getPrimeFactors(integer) {
    var primeArray = [];
    var isPrime;

    for (var i = 2; i <= integer; i++) {
        if (integer % i !== 0) continue;

        for (var j = 2; j <= i / 2; j++) {
            isPrime = i % j !== 0;
        }

        if (!isPrime) continue;
        integer /= i;
        primeArray.push(i);
    }

    return primeArray;
}

var maxFactors = 0;
var maxFactorsNum = 0;

function benchit() {
    maxFactors = 0;
    maxFactorsNum = 0;
    for (var i = 1; i < 2000; i++) {
        var factors = getPrimeFactors(i);
        if (factors.length > maxFactors) {
            maxFactors = factors.length;
            maxFactorsNum = i;
        }
    }
    return maxFactorsNum;
}

var RUNS = 10;
var EXPECTED = 0;

var result;
var start = performance.now();
for (var i = 0; i < RUNS; i++) {
    result = benchit();
    if (i === 0) EXPECTED = result;
}
var elapsed = performance.now() - start;

if (result !== EXPECTED) {
    log("FAIL: prime_factors = " + result + ", expected " + EXPECTED);
} else {
    log(EXPECTED + " (" + elapsed.toFixed(1) + "ms total, " + (elapsed / RUNS).toFixed(2) + "ms avg)");
}
