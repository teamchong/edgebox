// Data Processing Benchmark
// Realistic worker pattern: process arrays of numbers with multiple operations
// Combines array reads, arithmetic, comparisons, and bitwise ops

function arrayStats(data) {
  var sum = 0, min = data[0], max = data[0];
  for (var i = 0; i < data.length; i++) {
    sum = sum + data[i] | 0;
    if (data[i] < min) min = data[i];
    if (data[i] > max) max = data[i];
  }
  return sum + min + max | 0;
}

function binarySearch(arr, target) {
  var lo = 0, hi = arr.length - 1;
  while (lo <= hi) {
    var mid = (lo + hi) >>> 1;
    if (arr[mid] === target) return mid;
    if (arr[mid] < target) lo = mid + 1;
    else hi = mid - 1;
  }
  return -1;
}

function histogram(data, buckets, numBuckets) {
  for (var i = 0; i < data.length; i++) {
    var idx = data[i] % numBuckets;
    if (idx < 0) idx = idx + numBuckets;
    buckets[idx] = buckets[idx] + 1 | 0;
  }
  return buckets[0];
}

function prefixSum(arr) {
  for (var i = 1; i < arr.length; i++) {
    arr[i] = arr[i] + arr[i - 1] | 0;
  }
  return arr[arr.length - 1];
}

// Setup test data
var N = 10000;
var data = new Int32Array(N);
for (var i = 0; i < N; i++) data[i] = (i * 7 + 13) % 1000;
var sorted = new Int32Array(N);
for (var i = 0; i < N; i++) sorted[i] = i * 3;

// Warmup
for (var w = 0; w < 3; w++) {
  arrayStats(data);
  binarySearch(sorted, 15000);
}

// Benchmark arrayStats
var start = Date.now();
var result;
for (var i = 0; i < 100000; i++) {
  result = arrayStats(data);
}
var t1 = Date.now() - start;
console.log("arrayStats 100K: " + t1 + "ms (result=" + result + ")");

// Benchmark binarySearch
start = Date.now();
var found = 0;
for (var i = 0; i < 1000000; i++) {
  found = found + binarySearch(sorted, (i * 7) % (N * 3)) | 0;
}
var t2 = Date.now() - start;
console.log("binarySearch 1M: " + t2 + "ms (found=" + found + ")");

// Benchmark histogram
var buckets = new Int32Array(100);
start = Date.now();
for (var i = 0; i < 10000; i++) {
  for (var j = 0; j < 100; j++) buckets[j] = 0;
  histogram(data, buckets, 100);
}
var t3 = Date.now() - start;
console.log("histogram 10K: " + t3 + "ms (bucket0=" + buckets[0] + ")");

// Benchmark prefixSum
var prefix = new Int32Array(N);
start = Date.now();
for (var i = 0; i < 10000; i++) {
  for (var j = 0; j < N; j++) prefix[j] = data[j];
  prefixSum(prefix);
}
var t4 = Date.now() - start;
console.log("prefixSum 10K: " + t4 + "ms (last=" + prefix[N-1] + ")");
