// Tail recursive benchmark - sum(1..1000)
// Tests: function call overhead (fclosure opcode - NOT frozen yet)
// Goal: track progress on freezing recursive function calls

function sumTailRec(n, acc) {
    if (n <= 0) return acc;
    return sumTailRec(n - 1, acc + n);
}

var N = 1000;
var RUNS = 10;
var EXPECTED = N * (N + 1) / 2;  // sum(1..1000) = 500500
var log = typeof print === "function" ? print : console.log;

var times = [];
for (var i = 0; i < RUNS; i++) {
    var start = performance.now();
    var result = sumTailRec(N, 0);
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
