// Fibonacci benchmark - fib(45)
// Tests pure computation performance (no I/O, no allocations in hot path)
// fib(45) runs ~3-16s per runtime, making startup overhead < 1%

function fib(n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

const EXPECTED = 1134903170;
const result = fib(45);

if (result !== EXPECTED) {
    console.log(`FAIL: fib(45) = ${result}, expected ${EXPECTED}`);
    process.exit(1);
} else {
    console.log(result);
}
