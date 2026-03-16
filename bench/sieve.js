// Sieve of Eratosthenes — classic prime-counting benchmark
// Tests loop + array access patterns

// Pure numeric version (no arrays, using bitwise sieve in registers)
// This version packs 32 flags into one integer using bitwise ops
function sieveBitwise(limit) {
  // Count primes up to 'limit' using trial division
  var count = 0;
  var n = 2;
  while (n <= limit) {
    var isPrime = 1;
    var d = 2;
    while (d * d <= n) {
      if (n % d === 0) {
        isPrime = 0;
        d = n; // break
      }
      d = d + 1;
    }
    count = count + isPrime;
    n = n + 1;
  }
  return count;
}

// Tower of Hanoi (recursive, pure i32)
function hanoi(n, from, to, via) {
  if (n <= 0) return 0;
  var moves = hanoi(n - 1, from, via, to);
  moves = moves + 1;
  moves = moves + hanoi(n - 1, via, to, from);
  return moves;
}

// Matrix multiply 2x2 (pure arithmetic, used in fast Fibonacci)
function matMul(a, b, c, d, e, f, g, h) {
  // [a b] * [e f] = [ae+bg af+bh]
  // [c d]   [g h]   [ce+dg cf+dh]
  // Returns packed: (ae+bg) (only first element for benchmark)
  return (a * e + b * g) | 0;
}

// Fast power (recursive, i32)
function fastPow(base, exp) {
  if (exp === 0) return 1;
  if (exp === 1) return base;
  if (exp % 2 === 0) {
    var half = fastPow(base, exp / 2);
    return (half * half) | 0;
  }
  return (base * fastPow(base, exp - 1)) | 0;
}

// Digital root (repeated digit sum until single digit)
function digitalRoot(n) {
  while (n >= 10) {
    var sum = 0;
    while (n > 0) {
      sum = sum + (n % 10);
      n = (n / 10) | 0;
    }
    n = sum;
  }
  return n;
}

// === Benchmark runner ===
function bench(name, fn, iterations) {
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

console.log("=== Algorithm Benchmarks ===");

bench("sieveBitwise 10K", function() { return sieveBitwise(10000); }, 10);

bench("hanoi(25)", function() { return hanoi(25, 1, 3, 2); }, 1);

bench("matMul 10M", function() {
  var r = 1;
  for (var i = 0; i < 10000000; i++) {
    r = matMul(r, i, i+1, i+2, i+3, i+4, i+5, i+6);
  }
  return r;
}, 1);

bench("fastPow 10M", function() {
  var sum = 0;
  for (var i = 1; i <= 10000000; i++) {
    sum = (sum + fastPow(i % 100, i % 20)) | 0;
  }
  return sum;
}, 1);

bench("digitalRoot 1M", function() {
  var sum = 0;
  for (var i = 1; i <= 1000000; i++) {
    sum = sum + digitalRoot(i);
  }
  return sum;
}, 1);
