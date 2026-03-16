// Benchmark: functions that use arr.length (get_length opcode)
// These functions should compile to standalone WASM with extra length params

function arraySum(arr) {
  var sum = 0;
  for (var i = 0; i < arr.length; i++) {
    sum = sum + arr[i] | 0;
  }
  return sum;
}

function arrayMax(arr) {
  var max = arr[0];
  for (var i = 1; i < arr.length; i++) {
    if (arr[i] > max) max = arr[i];
  }
  return max;
}

function arrayReverse(arr) {
  var n = arr.length;
  for (var i = 0; i < (n >> 1); i++) {
    var tmp = arr[i];
    arr[i] = arr[n - 1 - i];
    arr[n - 1 - i] = tmp;
  }
  return arr[0];
}

function dotProduct(a, b) {
  var sum = 0;
  for (var i = 0; i < a.length; i++) {
    sum = sum + a[i] * b[i] | 0;
  }
  return sum;
}

// Setup test data
var arr = new Array(1000);
for (var i = 0; i < 1000; i++) arr[i] = i;

var a = new Array(1000);
var b = new Array(1000);
for (var i = 0; i < 1000; i++) { a[i] = i; b[i] = 1000 - i; }

// Warmup
for (var i = 0; i < 3; i++) {
  arraySum(arr);
  arrayMax(arr);
  dotProduct(a, b);
}

// Benchmark arraySum
var start = Date.now();
var result;
for (var i = 0; i < 1e5; i++) {
  result = arraySum(arr);
}
var t1 = Date.now() - start;
console.log("arraySum 100K: " + t1 + "ms (result=" + result + ")");

// Benchmark arrayMax
start = Date.now();
for (var i = 0; i < 1e5; i++) {
  result = arrayMax(arr);
}
var t2 = Date.now() - start;
console.log("arrayMax 100K: " + t2 + "ms (result=" + result + ")");

// Benchmark dotProduct
start = Date.now();
for (var i = 0; i < 1e5; i++) {
  result = dotProduct(a, b);
}
var t3 = Date.now() - start;
console.log("dotProduct 100K: " + t3 + "ms (result=" + result + ")");

// Benchmark arrayReverse
var rev = new Array(100);
for (var i = 0; i < 100; i++) rev[i] = i;
for (var i = 0; i < 3; i++) arrayReverse(rev);
start = Date.now();
for (var i = 0; i < 1e6; i++) {
  arrayReverse(rev);
}
var t4 = Date.now() - start;
console.log("arrayReverse 1M: " + t4 + "ms (result=" + rev[0] + "," + rev[1] + ")");
