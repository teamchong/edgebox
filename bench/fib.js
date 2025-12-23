// Fibonacci benchmark - fib(45) x 10
// Tests pure computation performance (no I/O, no allocations in hot path)
// Uses performance.now() to measure actual runtime, excluding startup

function fib(n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

const EXPECTED = 1134903170;
const RUNS = 10;
const times = [];

for (let i = 0; i < RUNS; i++) {
    const start = performance.now();
    const result = fib(45);
    const elapsed = performance.now() - start;
    times.push(elapsed);
    if (result !== EXPECTED) {
        console.log(`FAIL: fib(45) = ${result}, expected ${EXPECTED}`);
        if (typeof process !== 'undefined') process.exit(1);
    }
}

const avg = times.reduce((a, b) => a + b, 0) / times.length;
console.log(`${EXPECTED} (${avg.toFixed(2)}ms avg, ${times.map(t => t.toFixed(0)).join('/')})`);

