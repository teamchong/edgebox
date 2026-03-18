// Recursive Algorithm Benchmark
// Tests where WASM's fixed-cost call overhead wins:
// recursive tree traversal, divide-and-conquer, dynamic programming.
//
// V8 penalty: each recursive call goes through interpreter/deopt checks.
// WASM advantage: flat call stack, no type speculation overhead.
//
// Usage:
//   node bench/recursive.js
//   node zig-out/bin/bench/recursive.js/recursive-worker.mjs

function fib(n) {
  if (n <= 1) return n;
  return fib(n - 1) + fib(n - 2);
}

function ackermann(m, n) {
  if (m === 0) return n + 1;
  if (n === 0) return ackermann(m - 1, 1);
  return ackermann(m - 1, ackermann(m, n - 1));
}

// Tower of Hanoi — counts moves recursively
function hanoi(n, from, to, via) {
  if (n <= 0) return 0;
  return hanoi(n - 1, from, via, to) + 1 + hanoi(n - 1, via, to, from);
}

// Recursive power (fast exponentiation)
function power(base, exp) {
  if (exp === 0) return 1;
  if ((exp & 1) === 0) {
    var half = power(base, exp >> 1);
    return half * half | 0;
  }
  return base * power(base, exp - 1) | 0;
}

// Warmup
for (var w = 0; w < 20; w++) {
  fib(30);
  ackermann(3, 7);
  hanoi(20, 1, 3, 2);
  power(3, 20);
}

// Benchmark
var RUNS = 5;

var t0 = Date.now();
var result = 0;
for (var r = 0; r < RUNS; r++) result = result + fib(40) | 0;
console.log('fib(40): ' + (Date.now() - t0) + 'ms (result=' + result + ')');

t0 = Date.now();
result = 0;
for (var r = 0; r < 20; r++) result = result + ackermann(3, 10) | 0;
console.log('ackermann 3,10: ' + (Date.now() - t0) + 'ms (result=' + result + ')');

t0 = Date.now();
result = 0;
for (var r = 0; r < RUNS; r++) result = result + hanoi(25, 1, 3, 2) | 0;
console.log('hanoi 25: ' + (Date.now() - t0) + 'ms (result=' + result + ')');

t0 = Date.now();
result = 0;
for (var r = 0; r < 100000; r++) result = result + power(3, 30) | 0;
console.log('power 100K: ' + (Date.now() - t0) + 'ms (result=' + result + ')');
