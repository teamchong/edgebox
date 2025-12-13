// Iterative loop benchmark - tests array operations
// Uses for loop with array indexing (will fallback to interpreter due to unsupported opcodes)

function sumLoop(arr) {
    var acc = 0;
    for (var i = 0; i < arr.length; i++) {
        acc = acc + arr[i];
    }
    return acc;
}

var SIZE = 10000;
var EXPECTED = (SIZE - 1) * SIZE / 2;  // sum(0..SIZE-1)
var RUNS = 1000;
var total_time = 0;
var log = typeof print === 'function' ? print : console.log;

// Build array
var data = [];
for (var i = 0; i < SIZE; i++) data.push(i);

for (var i = 0; i < RUNS; i++) {
    var start = performance.now();
    var result = sumLoop(data);
    var elapsed = performance.now() - start;
    total_time += elapsed;
    if (result !== EXPECTED) {
        log("FAIL: sum = " + result + ", expected " + EXPECTED);
    }
}

var avg = total_time / RUNS;
log(EXPECTED + " (" + avg.toFixed(2) + "ms avg)");
