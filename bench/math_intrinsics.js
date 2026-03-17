// Math intrinsics benchmark — tests Math.pow/min/max/sqrt in compute loops
// These compile to WASM intrinsics via LLVM (pow→import, sqrt→f64.sqrt, etc.)

// Gamma correction: common in image processing
// Math.pow per element in a loop over Float64Array
function gammaCorrect(arr, gamma) {
  for (var i = 0; i < arr.length; i++) {
    arr[i] = Math.pow(arr[i] / 255.0, 1.0 / gamma) * 255.0;
  }
  return arr[0];
}

// Vector magnitude with Math.sqrt (already supported as WASM f64.sqrt)
function magnitudes(positions, out) {
  for (var i = 0; i < out.length; i++) {
    var x = positions[i * 3];
    var y = positions[i * 3 + 1];
    var z = positions[i * 3 + 2];
    out[i] = Math.sqrt(x * x + y * y + z * z);
  }
  return out[0];
}

// Clamp with Math.min/Math.max — common in graphics/audio
function clamp(arr, lo, hi) {
  for (var i = 0; i < arr.length; i++) {
    arr[i] = Math.max(lo, Math.min(hi, arr[i]));
  }
  return arr[0];
}

// Polynomial evaluation with Math.pow (synthetic but exercises pow heavily)
function polyEval(x, n) {
  return Math.pow(x, n) + 2 * Math.pow(x, n - 1) + 3;
}

// Setup
var N = 10000;
var pixels = new Float64Array(N);
var positions = new Float64Array(N * 3);
var mags = new Float64Array(N);
var values = new Float64Array(N);
for (var i = 0; i < N; i++) {
  pixels[i] = i % 256;
  values[i] = (i - N / 2) * 0.01;
}
for (var i = 0; i < N * 3; i++) positions[i] = (i * 7 + 13) / 1000;

// Warmup
for (var w = 0; w < 10; w++) gammaCorrect(pixels, 2.2);
for (var i = 0; i < N; i++) pixels[i] = i % 256;
for (var w = 0; w < 10; w++) magnitudes(positions, mags);
for (var w = 0; w < 10; w++) clamp(values, -1.0, 1.0);
for (var i = 0; i < N; i++) values[i] = (i - N / 2) * 0.01;

// Benchmark gammaCorrect
var start = Date.now();
for (var iter = 0; iter < 10000; iter++) {
  for (var j = 0; j < N; j++) pixels[j] = j % 256;
  gammaCorrect(pixels, 2.2);
}
var t1 = Date.now() - start;
console.log("gammaCorrect 10K: " + t1 + "ms (pixel[128]=" + pixels[128].toFixed(4) + ")");

// Benchmark magnitudes
start = Date.now();
for (var iter = 0; iter < 100000; iter++) {
  magnitudes(positions, mags);
}
var t2 = Date.now() - start;
console.log("magnitudes 100K: " + t2 + "ms (mag[0]=" + mags[0].toFixed(6) + ")");

// Benchmark clamp
start = Date.now();
for (var iter = 0; iter < 100000; iter++) {
  for (var j = 0; j < N; j++) values[j] = (j - N / 2) * 0.01;
  clamp(values, -1.0, 1.0);
}
var t3 = Date.now() - start;
console.log("clamp 100K: " + t3 + "ms (val[0]=" + values[0].toFixed(6) + ")");

// Benchmark polyEval (scalar, no arrays)
for (var w = 0; w < 5; w++) polyEval(1.5, 3);
start = Date.now();
var result = 0;
for (var i = 0; i < 1000000; i++) {
  result = polyEval(1.5 + (i % 10) * 0.01, 3);
}
var t4 = Date.now() - start;
console.log("polyEval 1M: " + t4 + "ms (result=" + result.toFixed(6) + ")");
