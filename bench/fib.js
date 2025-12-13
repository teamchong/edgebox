// Fibonacci benchmark - fib(45)
// Tests: frozen recursive computation (no I/O, no allocations)

function fib(n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

var RUNS = 10;
var EXPECTED = 1134903170;
var log = typeof print === "function" ? print : console.log;

var times = [];
for (var i = 0; i < RUNS; i++) {
    var start = performance.now();
    var result = fib(45);
    times.push(performance.now() - start);
}

// Calculate avg without reduce (for Porffor compatibility)
var total = 0;
for (var j = 0; j < times.length; j++) {
    total = total + times[j];
}
var avg = total / times.length;

if (result !== EXPECTED) {
    log("FAIL: got " + result + ", expected " + EXPECTED);
} else {
    log(EXPECTED + " (" + avg.toFixed(2) + "ms avg)");
}
