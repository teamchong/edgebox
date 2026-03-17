// Sorting Benchmark
// Bun claims ~2x over Node.js for sorting 100k numbers.
// EdgeBox compiles sort kernels to WASM — should beat both.
//
// Uses insertion sort and merge sort on Int32Array.
// These are pure numeric (comparisons, swaps, array access) — ideal for AOT.

function insertionSort(arr) {
  for (var i = 1; i < arr.length; i++) {
    var key = arr[i];
    var j = i - 1;
    while (j >= 0 && arr[j] > key) {
      arr[j + 1] = arr[j];
      j = j - 1;
    }
    arr[j + 1] = key;
  }
  return arr[0];
}

function mergeSortHelper(arr, tmp, lo, hi) {
  if (hi - lo <= 1) return 0;
  var mid = (lo + hi) >>> 1;
  mergeSortHelper(arr, tmp, lo, mid);
  mergeSortHelper(arr, tmp, mid, hi);
  // Merge
  var i = lo, j = mid, k = lo;
  while (i < mid && j < hi) {
    if (arr[i] <= arr[j]) {
      tmp[k] = arr[i];
      i = i + 1;
    } else {
      tmp[k] = arr[j];
      j = j + 1;
    }
    k = k + 1;
  }
  while (i < mid) { tmp[k] = arr[i]; i = i + 1; k = k + 1; }
  while (j < hi) { tmp[k] = arr[j]; j = j + 1; k = k + 1; }
  // Copy back
  for (var x = lo; x < hi; x++) arr[x] = tmp[x];
  return 0;
}

// Setup
var N = 10000;
var data = new Int32Array(N);
var tmp = new Int32Array(N);
var copy = new Int32Array(N);

// Seed with pseudo-random data
for (var i = 0; i < N; i++) data[i] = (i * 2654435761 + 17) | 0;

// Warmup
for (var w = 0; w < 3; w++) {
  for (var j = 0; j < N; j++) copy[j] = data[j];
  insertionSort(copy);
}

// Benchmark insertion sort (O(n^2) — compute-heavy)
var start = Date.now();
for (var r = 0; r < 100; r++) {
  for (var j = 0; j < N; j++) copy[j] = data[j];
  insertionSort(copy);
}
var t1 = Date.now() - start;
console.log("insertionSort 10K: " + t1 + "ms (first=" + copy[0] + ")");

// Benchmark merge sort (O(n log n) — recursive + array heavy)
start = Date.now();
for (var r = 0; r < 1000; r++) {
  for (var j = 0; j < N; j++) copy[j] = data[j];
  mergeSortHelper(copy, tmp, 0, N);
}
var t2 = Date.now() - start;
console.log("mergeSort 10K: " + t2 + "ms (first=" + copy[0] + ")");
