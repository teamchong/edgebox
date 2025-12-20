// Greatest Common Divisor benchmark - tests multi-arg int32 optimization
// Classic Euclidean algorithm: gcd(a, b) = gcd(b, a % b)
// Tests: 2-arg self-recursive function with int32 operations

function gcd(a, b) {
    if (b === 0) return a;
    return gcd(b, a % b);
}

var RUNS = 1000000;
var log = typeof print === "function" ? print : console.log;

var times = [];
for (var i = 0; i < 10; i++) {
    var start = performance.now();
    for (var j = 0; j < RUNS; j++) {
        var result = gcd(1071, 462);  // Should be 21
    }
    times.push(performance.now() - start);
}

// Calculate avg without reduce (for compatibility)
var total = 0;
for (var j = 0; j < times.length; j++) {
    total = total + times[j];
}
var avg = total / times.length;

if (result !== 21) {
    log("FAIL: got " + result + ", expected 21");
} else {
    log("gcd(1071, 462) = " + result + " (" + avg.toFixed(2) + "ms avg, " + RUNS + " iterations)");
}
