// Mandelbrot Set Benchmark - No closures version
// Modified to pass all values as arguments to avoid closure issues

var log = typeof print === "function" ? print : console.log;

var SIZE = 200;
var MAX_ITER = 100;
var pixels = new Uint8ClampedArray(SIZE * SIZE * 4);

// Pure function - no closure vars, can be frozen
function mandelbrot_compute(pixels, size, max_iter) {
    for (var x = 0; x < size; x++) {
        for (var y = 0; y < size; y++) {
            var a = 4 * x / size - 2;
            var b = 4 * y / size - 2;
            var iter = 0;
            var ca = a;
            var cb = b;
            while (iter < max_iter) {
                var aa = a * a - b * b;
                var bb = 2 * a * b;
                a = aa + ca;
                b = bb + cb;
                if (Math.abs(a + b) > 16) {
                    break;
                }
                iter++;
            }
            var bright = iter / max_iter;
            bright = Math.sqrt(bright) * 150;
            if (iter === max_iter) {
                bright = 255;
            }
            var p = (x + y * size) << 2;
            pixels[p + 0] = 255 - bright;
            pixels[p + 1] = 255 - bright;
            pixels[p + 2] = 255 - bright;
        }
    }
}

function benchit_compute(pixels, size, max_iter) {
    mandelbrot_compute(pixels, size, max_iter);
    var checksum = 0;
    for (var i = 0; i < pixels.length; i += 4) {
        checksum += pixels[i];
    }
    return checksum;
}

var RUNS = 5;
var EXPECTED = 0;

var result;
var start = performance.now();
for (var i = 0; i < RUNS; i++) {
    result = benchit_compute(pixels, SIZE, MAX_ITER);
    if (i === 0) EXPECTED = result;
}
var elapsed = performance.now() - start;

if (result !== EXPECTED) {
    log("FAIL: mandelbrot checksum = " + result + ", expected " + EXPECTED);
} else {
    log(EXPECTED + " (" + elapsed.toFixed(1) + "ms total, " + (elapsed / RUNS).toFixed(2) + "ms avg)");
}
