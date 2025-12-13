// Fibonacci benchmark - fib(45)
// Tests: frozen recursive computation (no I/O, no allocations)

function fib(n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

var RUNS = 10;
var EXPECTED = 1134903170;
var log = typeof print === 'function' ? print : console.log;

var times = [];
for (var i = 0; i < RUNS; i++) {
    var start = performance.now();
    var result = fib(45);
    times.push(performance.now() - start);
}

if (result !== EXPECTED) {
    log('FAIL: got ' + result + ', expected ' + EXPECTED);
} else {
    var avg = times.reduce(function(a, b) { return a + b; }, 0) / times.length;
    log(EXPECTED + ' (' + avg.toFixed(2) + 'ms avg)');
}
