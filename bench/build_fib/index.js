// Fibonacci benchmark - CPU-intensive recursive calculation
function fib(n) {
    if (n < 2) return n;
    return fib(n - 1) + fib(n - 2);
}

const iterations = 5;
const times = [];

for (let i = 0; i < iterations; i++) {
    const start = Date.now();
    fib(25);
    times.push(Date.now() - start);
}

times.sort((a, b) => a - b);

const p50 = times[Math.floor(iterations * 0.50)];
const p90 = times[Math.floor(iterations * 0.90)];

console.log(`Fib(25) x${iterations}: P50=${p50}ms P90=${p90}ms`);
