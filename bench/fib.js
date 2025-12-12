// Fibonacci benchmark - fib(45)
// Tests pure computation performance (no I/O, no allocations in hot path)
// Uses performance.now() to measure actual runtime, excluding startup

function fib(n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

const EXPECTED = 1134903170;

const start = performance.now();
const result = fib(45);
const elapsed = performance.now() - start;

if (result !== EXPECTED) {
    console.log(`FAIL: fib(45) = ${result}, expected ${EXPECTED}`);
    if (typeof process !== 'undefined') process.exit(1);
} else {
    console.log(`${result} (${elapsed.toFixed(2)}ms)`);
}
