// Mandelbrot Set Benchmark
// Tests: floating-point math, complex number operations, nested loops
// Computes a 200x200 mandelbrot set with max 100 iterations per pixel

var log = typeof print === "function" ? print : console.log;

function mandelbrot(width, height, maxIter) {
    var count = 0;
    for (var py = 0; py < height; py++) {
        for (var px = 0; px < width; px++) {
            var x0 = (px / width) * 3.5 - 2.5;
            var y0 = (py / height) * 2.0 - 1.0;
            var x = 0.0;
            var y = 0.0;
            var iter = 0;
            while (x * x + y * y <= 4.0 && iter < maxIter) {
                var xtemp = x * x - y * y + x0;
                y = 2.0 * x * y + y0;
                x = xtemp;
                iter++;
            }
            count += iter;
        }
    }
    return count;
}

var WIDTH = 200;
var HEIGHT = 200;
var MAX_ITER = 100;
var RUNS = 200;
var EXPECTED = 1047372;

// Measure total time for ALL runs (only 2 performance.now calls)
var result;
var start = performance.now();
for (var i = 0; i < RUNS; i++) {
    result = mandelbrot(WIDTH, HEIGHT, MAX_ITER);
}
var elapsed = performance.now() - start;

if (result !== EXPECTED) {
    log("FAIL: mandelbrot = " + result + ", expected " + EXPECTED);
} else {
    log(EXPECTED + " (" + elapsed.toFixed(1) + "ms total, " + (elapsed / RUNS).toFixed(2) + "ms avg)");
}
