// Allocator stress test - measures peak memory usage
// Shows allocator efficiency across different JS runtimes

const ITERATIONS = 100000;  // 100k iterations = 300k objects
const RUNS = 5;

// Get memory usage (works in Node.js, Bun, and EdgeBox)
function getMemoryMB() {
    if (typeof process !== 'undefined' && process.memoryUsage) {
        return process.memoryUsage().rss / 1024 / 1024;
    }
    return 0;
}

function runAlloc() {
    const objects = [];
    for (let i = 0; i < ITERATIONS; i++) {
        objects.push({ id: i, data: "x".repeat(32) });   // ~64B
        objects.push(new Array(16).fill(i));              // ~128B
        objects.push("string-" + i);                      // ~32B
    }
    return objects.length;
}

const memories = [];
let peakMem = 0;

for (let r = 0; r < RUNS; r++) {
    const len = runAlloc();
    const mem = getMemoryMB();
    memories.push(mem);
    if (mem > peakMem) peakMem = mem;

    if (len !== ITERATIONS * 3) {
        console.log(`FAIL: ${len} objects, expected ${ITERATIONS * 3}`);
        if (typeof process !== 'undefined' && process.exit) process.exit(1);
    }
}

const avgMem = memories.reduce((a, b) => a + b, 0) / memories.length;
console.log(`${ITERATIONS * 3} allocs x${RUNS} (peak: ${peakMem.toFixed(1)}MB, avg: ${avgMem.toFixed(1)}MB)`);
