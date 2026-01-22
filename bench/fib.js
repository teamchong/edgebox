function fib(n) {
  if (n <= 1) return n;
  return fib(n - 1) + fib(n - 2);
}

const start = Date.now();
const result = fib(45);
const elapsed = Date.now() - start;
console.log(`fib(45): ${result} time: ${elapsed}ms`);
