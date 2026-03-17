// f64+array benchmark — compute-heavy float operations on typed arrays
// Tests Float64Array WASM pipeline: f64 tier with array access
// LLVM auto-vectorizes these to WASM SIMD (f64x2.mul, f64x2.add)

// Normalized dot product (division triggers f64 tier)
function dotProductNorm(a, b, len) {
  var sum = 0;
  for (var i = 0; i < len; i++) {
    sum = sum + a[i] * b[i];
  }
  return sum / len;
}

// Euclidean distance (Math.sqrt triggers f64 tier)
function euclideanDist(a, b, len) {
  var sum = 0;
  for (var i = 0; i < len; i++) {
    var diff = a[i] - b[i];
    sum = sum + diff * diff;
  }
  return Math.sqrt(sum);
}

// Setup — Float64Array inputs
var N = 10000;
var a = new Float64Array(N);
var b = new Float64Array(N);
for (var i = 0; i < N; i++) {
  a[i] = (i * 7 + 13) / 1000;
  b[i] = (i * 3 + 17) / 1000;
}

// Warmup
for (var w = 0; w < 10; w++) dotProductNorm(a, b, N);

var start = Date.now();
var result;
for (var i = 0; i < 100000; i++) {
  result = dotProductNorm(a, b, N);
}
var t1 = Date.now() - start;
console.log("dotProductNorm 100K: " + t1 + "ms (result=" + result + ")");

// Euclidean distance
for (var w = 0; w < 10; w++) euclideanDist(a, b, N);

start = Date.now();
for (var i = 0; i < 100000; i++) {
  result = euclideanDist(a, b, N);
}
var t2 = Date.now() - start;
console.log("euclideanDist 100K: " + t2 + "ms (result=" + result + ")");
