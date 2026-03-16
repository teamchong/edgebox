// Compute-Heavy Benchmark Suite
// Tests pure numeric compute (where AOT compilation shines)
// vs V8 JIT. Each function is self-contained — no external deps.

// 1. Fibonacci (recursive, i32)
function fib(n) {
  if (n <= 1) return n;
  return fib(n - 1) + fib(n - 2);
}

// 2. Hash mixing (bitwise i32, used in hash tables)
function hashMix(a, b, c) {
  a = a - b; a = a - c; a = a ^ (c >>> 13);
  b = b - c; b = b - a; b = b ^ (a << 8);
  c = c - a; c = c - b; c = c ^ (b >>> 13);
  a = a - b; a = a - c; a = a ^ (c >>> 12);
  b = b - c; b = b - a; b = b ^ (a << 16);
  c = c - a; c = c - b; c = c ^ (b >>> 5);
  a = a - b; a = a - c; a = a ^ (c >>> 3);
  b = b - c; b = b - a; b = b ^ (a << 10);
  c = c - a; c = c - b; c = c ^ (b >>> 15);
  return c;
}

// 3. isPrime (i32, loop with modulo)
function isPrime(n) {
  if (n < 2) return 0;
  if (n < 4) return 1;
  if (n % 2 === 0) return 0;
  var i = 3;
  while (i * i <= n) {
    if (n % i === 0) return 0;
    i = i + 2;
  }
  return 1;
}

// 4. GCD (recursive, i32)
function gcd(a, b) {
  if (b === 0) return a;
  return gcd(b, a % b);
}

// 5. Ackermann (deeply recursive, i32)
function ackermann(m, n) {
  if (m === 0) return n + 1;
  if (n === 0) return ackermann(m - 1, 1);
  return ackermann(m - 1, ackermann(m, n - 1));
}

// 6. Collatz steps (loop, i32)
function collatzSteps(n) {
  var steps = 0;
  while (n !== 1) {
    if (n % 2 === 0) {
      n = n / 2;
    } else {
      n = 3 * n + 1;
    }
    steps = steps + 1;
  }
  return steps;
}

// 7. N-body distance (f64 division)
function nbodyEnergy(x1, y1, x2, y2) {
  var dx = x2 - x1;
  var dy = y2 - y1;
  return dx * dx + dy * dy;
}

// 8. Count primes (cross-function call: countPrimes → isPrime)
function countPrimes(n) {
  var count = 0;
  var i = 2;
  while (i < n) {
    count = count + isPrime(i);
    i = i + 1;
  }
  return count;
}

// === Run benchmarks ===
function bench(name, fn, iterations) {
  // Warmup
  for (var i = 0; i < 3; i++) fn();

  var start = Date.now();
  var result;
  for (var i = 0; i < iterations; i++) {
    result = fn();
  }
  var elapsed = Date.now() - start;
  console.log(name + ": " + elapsed + "ms (" + iterations + " runs, result=" + result + ")");
  return elapsed;
}

console.log("=== Compute-Heavy Benchmarks ===");

bench("fib(40)", function() { return fib(40); }, 3);

bench("hashMix 10M", function() {
  var h = 0;
  for (var i = 0; i < 10000000; i++) {
    h = hashMix(h, i, 12345);
  }
  return h;
}, 1);

bench("isPrime 100K", function() {
  var count = 0;
  for (var i = 0; i < 100000; i++) {
    count = count + isPrime(i);
  }
  return count;
}, 1);

bench("gcd 1M", function() {
  var sum = 0;
  for (var i = 1; i < 1000000; i++) {
    sum = sum + gcd(i, 12345678);
  }
  return sum;
}, 1);

bench("ackermann(3,10)", function() { return ackermann(3, 10); }, 10);

bench("collatz 100K", function() {
  var total = 0;
  for (var i = 1; i < 100000; i++) {
    total = total + collatzSteps(i);
  }
  return total;
}, 1);

bench("nbodyDist 10M", function() {
  var e = 0;
  for (var i = 0; i < 10000000; i++) {
    e = e + nbodyEnergy(i, i + 1, i + 2, i + 3);
  }
  return e;
}, 1);

bench("countPrimes 100K", function() { return countPrimes(100000); }, 1);
