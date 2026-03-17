// Matrix Multiplication Benchmark
// Classic compute workload — the reason people reach for C/Rust/Go.
// Dense matrix multiply is O(n^3) pure arithmetic with array access.
// WASM LLVM backend auto-vectorizes the inner loop.
//
// Uses __wasmArray for zero-copy when running under EdgeBox AOT+JIT.
// Falls back to normal Float64Array on plain Node.js.

function matmul(a, b, c, n) {
  for (var i = 0; i < n; i++) {
    for (var j = 0; j < n; j++) {
      var sum = 0.0;
      for (var k = 0; k < n; k++) {
        sum = sum + a[i * n + k] * b[k * n + j];
      }
      c[i * n + j] = sum;
    }
  }
  return c[0];
}

function matvec(a, x, y, n) {
  for (var i = 0; i < n; i++) {
    var sum = 0.0;
    for (var j = 0; j < n; j++) {
      sum = sum + a[i * n + j] * x[j];
    }
    y[i] = sum;
  }
  return y[0];
}

// Setup: 128x128 matrices (16K elements each)
var N = 128;
var a, b, c, x, y;
if (typeof __wasmArray === 'function') {
  a = __wasmArray(Float64Array, N * N);
  b = __wasmArray(Float64Array, N * N);
  c = __wasmArray(Float64Array, N * N);
  x = __wasmArray(Float64Array, N);
  y = __wasmArray(Float64Array, N);
} else {
  a = new Float64Array(N * N);
  b = new Float64Array(N * N);
  c = new Float64Array(N * N);
  x = new Float64Array(N);
  y = new Float64Array(N);
}

for (var i = 0; i < N * N; i++) {
  a[i] = (i % 17) * 0.1;
  b[i] = (i % 13) * 0.1;
}
for (var i = 0; i < N; i++) x[i] = i * 0.01;

// Warmup
for (var w = 0; w < 3; w++) {
  matmul(a, b, c, N);
  matvec(a, x, y, N);
}

// Benchmark matmul (128x128, ~4M flops per call)
var start = Date.now();
for (var r = 0; r < 100; r++) {
  matmul(a, b, c, N);
}
var t1 = Date.now() - start;
console.log("matmul 128x128: " + t1 + "ms (result=" + c[0].toFixed(4) + ")");

// Benchmark matvec (128x128 * 128, cheaper per call)
start = Date.now();
for (var r = 0; r < 10000; r++) {
  matvec(a, x, y, N);
}
var t2 = Date.now() - start;
console.log("matvec 128: " + t2 + "ms (result=" + y[0].toFixed(4) + ")");
