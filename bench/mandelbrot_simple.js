// Simple mandelbrot trace
var SIZE = 3;

function mandelbrot_compute(pixels, size, max_iter) {
    console.log("size=" + size + " max_iter=" + max_iter);
    for (var x = 0; x < size; x++) {
        for (var y = 0; y < size; y++) {
            console.log("x=" + x + " y=" + y);
            var a = 4 * x / size - 2;
            console.log("  a=" + a);
        }
    }
}

var pixels = [];
mandelbrot_compute(pixels, SIZE, 5);
console.log("Done");
