// Fibonacci benchmark - CPU-intensive recursive calculation
// Verifies correctness and measures execution time

function fib(n) {
    if (n < 2) return n;
    return fib(n - 1) + fib(n - 2);
}

// Verify correctness first
const EXPECTED_FIB_35 = 9227465;
const result = fib(35);

if (result !== EXPECTED_FIB_35) {
    console.log(`FAIL: fib(35) = ${result}, expected ${EXPECTED_FIB_35}`);
    if (typeof process !== 'undefined' && process.exit) process.exit(1);
} else {
    console.log(`OK: fib(35) = ${result}`);
}
