// Fibonacci benchmark - fib(40)
// Tests pure computation performance (no I/O, no allocations in hot path)

function fib(n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

const EXPECTED = 102334155;
const result = fib(40);

if (result !== EXPECTED) {
    console.log(`FAIL: fib(40) = ${result}, expected ${EXPECTED}`);
    process.exit(1);
} else {
    console.log(result);
}
