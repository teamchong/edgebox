// Gaussian Blur Benchmark
// Tests: 2D array access, floating-point math, convolution
// Applies a 5x5 Gaussian blur kernel to a 100x100 image

var log = typeof print === "function" ? print : console.log;

// 5x5 Gaussian kernel (sigma ~= 1.0)
var kernel = [
    [1, 4, 6, 4, 1],
    [4, 16, 24, 16, 4],
    [6, 24, 36, 24, 6],
    [4, 16, 24, 16, 4],
    [1, 4, 6, 4, 1]
];
var kernelSum = 256;  // Sum of all kernel values

function createImage(width, height) {
    var img = new Array(height);
    for (var y = 0; y < height; y++) {
        img[y] = new Array(width);
        for (var x = 0; x < width; x++) {
            // Checkerboard pattern
            img[y][x] = ((x + y) % 2) * 255;
        }
    }
    return img;
}

function gaussianBlur(img, width, height) {
    var output = new Array(height);
    for (var y = 0; y < height; y++) {
        output[y] = new Array(width);
    }

    for (var y = 2; y < height - 2; y++) {
        for (var x = 2; x < width - 2; x++) {
            var sum = 0;
            for (var ky = 0; ky < 5; ky++) {
                for (var kx = 0; kx < 5; kx++) {
                    sum += img[y + ky - 2][x + kx - 2] * kernel[ky][kx];
                }
            }
            output[y][x] = Math.floor(sum / kernelSum);
        }
    }
    return output;
}

function checksum(img, width, height) {
    var sum = 0;
    for (var y = 2; y < height - 2; y++) {
        for (var x = 2; x < width - 2; x++) {
            sum += img[y][x];
        }
    }
    return sum;
}

var WIDTH = 100;
var HEIGHT = 100;
var RUNS = 1000;
var EXPECTED = 1170432;  // Pre-computed checksum

var times = [];
var img = createImage(WIDTH, HEIGHT);
var result;

for (var i = 0; i < RUNS; i++) {
    var start = performance.now();
    var blurred = gaussianBlur(img, WIDTH, HEIGHT);
    result = checksum(blurred, WIDTH, HEIGHT);
    times.push(performance.now() - start);
}

var total = 0;
for (var i = 0; i < times.length; i++) total += times[i];
var avg = total / times.length;

if (result !== EXPECTED) {
    log("FAIL: gaussian_blur checksum = " + result + ", expected " + EXPECTED);
} else {
    log(EXPECTED + " (" + avg.toFixed(2) + "ms avg, " + times.map(function(t) { return t.toFixed(1); }).join("/") + ")");
}
