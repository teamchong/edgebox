// Allocator stress test - measures malloc/free performance
// Uses performance.now() to measure actual runtime, excluding startup

const ITERATIONS = 10000;
const RUNS = 10;
const times = [];

function runAlloc() {
    const objects = [];
    for (let i = 0; i < ITERATIONS; i++) {
        // Mix of allocation sizes (matches QuickJS patterns)
        objects.push({ id: i, data: "x".repeat(32) });   // ~64B - small object
        objects.push(new Array(16).fill(i));              // ~128B - array
        objects.push("string-" + i);                      // ~32B - string

        // Periodic cleanup (triggers GC)
        if (i % 1000 === 0) {
            objects.length = 0;
        }
    }
    return objects.length;
}

for (let i = 0; i < RUNS; i++) {
    const start = performance.now();
    const len = runAlloc();
    const elapsed = performance.now() - start;
    times.push(elapsed);

    // Verify correctness
    const expectedLen = (ITERATIONS % 1000) * 3;
    if (len !== expectedLen) {
        console.log(`FAIL: objects.length = ${len}, expected ${expectedLen}`);
        if (typeof process !== 'undefined') process.exit(1);
    }
}

const avg = times.reduce((a, b) => a + b, 0) / times.length;
console.log(`${ITERATIONS * 3} allocs x${RUNS} (${avg.toFixed(2)}ms avg, ${times.map(t => t.toFixed(0)).join('/')})`);
