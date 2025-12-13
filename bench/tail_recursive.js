// Iterative sum benchmark - proves frozen interpreter is general-purpose
// Uses array iteration + accumulation to demonstrate it works with any algorithm
// Different pattern from fib() (iteration vs tree recursion)

function sumIterative(arr) {
    let acc = 0;
    for (let i = 0; i < arr.length; i++) {
        acc = acc + arr[i];
    }
    return acc;
}

const SIZE = 100000;  // 100k elements
const EXPECTED = (SIZE - 1) * SIZE / 2;  // Sum of 0..SIZE-1 = n*(n-1)/2
const RUNS = 100;
const times = [];

// Build array
const data = [];
for (let i = 0; i < SIZE; i++) data.push(i);

for (let i = 0; i < RUNS; i++) {
    const start = performance.now();
    const result = sumIterative(data);
    const elapsed = performance.now() - start;
    times.push(elapsed);
    if (result !== EXPECTED) {
        console.log(`FAIL: sum = ${result}, expected ${EXPECTED}`);
        if (typeof process !== 'undefined') process.exit(1);
    }
}

const avg = times.reduce((a, b) => a + b, 0) / times.length;
console.log(`${EXPECTED} (${avg.toFixed(2)}ms avg, ${times.slice(0, 10).map(t => t.toFixed(1)).join('/')}...)`);
