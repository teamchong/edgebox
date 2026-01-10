// Mandelbrot Set Benchmark
// Original from https://github.com/vExcess/js-engine-bench (600x600, 200 iter)
// Scaled down to 200x200, 100 iter for EdgeBox

var log = typeof print === "function" ? print : console.log;

var SIZE = 200;
var MAX_ITER = 100;
var pixels = new Uint8ClampedArray(SIZE * SIZE * 4);
var _abs = Math.abs;
var _sqrt = Math.sqrt;

function benchit() {
    for (var x = 0; x < SIZE; x++) {
        for (var y = 0; y < SIZE; y++) {
            var a = 4 * x / SIZE - 2;
            var b = 4 * y / SIZE - 2;
            var iter = 0;
            var ca = a;
            var cb = b;
            while (iter < MAX_ITER) {
                var aa = a * a - b * b;
                var bb = 2 * a * b;
                a = aa + ca;
                b = bb + cb;
                if (_abs(a + b) > 16) {
                    break;
                }
                iter++;
            }
            var bright = iter / MAX_ITER;
            bright = _sqrt(bright) * 150;
            if (iter === MAX_ITER) {
                bright = 255;
            }
            var p = (x + y * SIZE) << 2;
            pixels[p + 0] = 255 - bright;
            pixels[p + 1] = 255 - bright;
            pixels[p + 2] = 255 - bright;
        }
    }
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
    result = benchit();
    if (i === 0) EXPECTED = result;
}
var elapsed = performance.now() - start;

if (result !== EXPECTED) {
    log("FAIL: mandelbrot checksum = " + result + ", expected " + EXPECTED);
} else {
    log(EXPECTED + " (" + elapsed.toFixed(1) + "ms total, " + (elapsed / RUNS).toFixed(2) + "ms avg)");
}
