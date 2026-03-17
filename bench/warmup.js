// Warmup Penalty Benchmark
// Measures per-call latency across first 200 calls of compute-heavy functions.
// Shows the transition from cold → warm for both Node.js and EdgeBox.
//
// Stock Node.js: interpreter → Sparkplug → Maglev → TurboFan (1000+ calls)
// EdgeBox: WASM pre-compiled, V8 inlines trampoline after ~10 calls
//
// Usage:
//   node bench/warmup.js                     # Stock Node.js
//   node zig-out/bin/bench/warmup.js/warmup-worker.mjs  # EdgeBox AOT+JIT

function adler32(data, len) {
  var a = 1, b = 0;
  for (var i = 0; i < len; i++) {
    a = (a + data[i]) % 65521;
    b = (b + a) % 65521;
  }
  return (b << 16) | a;
}

function fib(n) {
  if (n <= 1) return n;
  return fib(n - 1) + fib(n - 2);
}

// Setup
var N = 10000;
var data = new Int32Array(N);
for (var i = 0; i < N; i++) data[i] = (i * 7 + 13) & 0xFF;

// Measure first 200 calls — enough to see TurboFan tier-up
var CALLS = 200;
var times = [];

for (var call = 0; call < CALLS; call++) {
  var start = performance.now();
  // Heavier compute: adler32 * 10 + fib(25) per call
  var r = 0;
  for (var rep = 0; rep < 10; rep++) r = r + adler32(data, N) | 0;
  r = r + fib(25) | 0;
  var elapsed = performance.now() - start;
  times.push(elapsed);
}

// Bucket into windows of 10 calls
console.log('window,avg_ms');
for (var w = 0; w < CALLS / 10; w++) {
  var sum = 0;
  for (var i = w * 10; i < (w + 1) * 10; i++) sum += times[i];
  console.log((w * 10) + '-' + ((w + 1) * 10 - 1) + ',' + (sum / 10).toFixed(3));
}

// Summary: first 10 vs last 10
var first10 = 0, last10 = 0;
for (var i = 0; i < 10; i++) first10 += times[i];
for (var i = CALLS - 10; i < CALLS; i++) last10 += times[i];
first10 /= 10;
last10 /= 10;

// Calls 10-20 (after initial JIT but before full optimization)
var mid10 = 0;
for (var i = 10; i < 20; i++) mid10 += times[i];
mid10 /= 10;

console.log('');
console.log('warmup_calls_0_9: ' + first10.toFixed(3) + 'ms');
console.log('warmup_calls_10_19: ' + mid10.toFixed(3) + 'ms');
console.log('warmup_calls_190_199: ' + last10.toFixed(3) + 'ms');
console.log('warmup_ratio: ' + (first10 / last10).toFixed(2) + 'x (first/last)');
console.log('warmup_steady_ms: ' + last10.toFixed(3) + 'ms');
