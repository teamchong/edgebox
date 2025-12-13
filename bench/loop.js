// Loop benchmark - array sum
// Tests: frozen array iteration (get_array_el, get_length opcodes)

function sumArray(arr) {
    var acc = 0;
    for (var i = 0; i < arr.length; i++) {
        acc = acc + arr[i];
    }
    return acc;
}

var SIZE = 10000;
var RUNS = 10;
var EXPECTED = (SIZE - 1) * SIZE / 2;  // sum(0..9999) = 49995000
var log = typeof print === "function" ? print : console.log;

// Build array once
var data = [];
for (var i = 0; i < SIZE; i++) data.push(i);

var times = [];
for (var i = 0; i < RUNS; i++) {
    var start = performance.now();
    var result = sumArray(data);
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
