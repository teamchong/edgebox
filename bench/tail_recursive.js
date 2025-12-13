// Tail recursive benchmark - sum(1..1000)
// Tests: function call overhead (fclosure opcode - NOT frozen yet)
// Goal: track progress on freezing recursive function calls

function sumTailRec(n, acc) {
    if (n <= 0) return acc;
    return sumTailRec(n - 1, acc + n);
}

function sum(n) {
    return sumTailRec(n, 0);
}

var N = 1000;
var RUNS = 10;
var EXPECTED = N * (N + 1) / 2;  // sum(1..1000) = 500500
var log = typeof print === 'function' ? print : console.log;

var times = [];
for (var i = 0; i < RUNS; i++) {
    var start = performance.now();
    var result = sum(N);
    times.push(performance.now() - start);
}

if (result !== EXPECTED) {
    log('FAIL: got ' + result + ', expected ' + EXPECTED);
} else {
    var avg = times.reduce(function(a, b) { return a + b; }, 0) / times.length;
    log(EXPECTED + ' (' + avg.toFixed(2) + 'ms avg)');
}
