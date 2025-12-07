// Allocator stress test - measures malloc/free performance
const iterations = 10000;
const objects = [];

const start = Date.now();
for (let i = 0; i < iterations; i++) {
    // Mix of allocation sizes (matches QuickJS patterns)
    objects.push({ id: i, data: "x".repeat(32) });   // ~64B - small object
    objects.push(new Array(16).fill(i));              // ~128B - array
    objects.push("string-" + i);                      // ~32B - string

    // Periodic cleanup (triggers GC)
    if (i % 1000 === 0) {
        objects.length = 0;
    }
}

const elapsed = Date.now() - start;
const allocsPerSec = Math.round((iterations * 3) / (elapsed / 1000));

console.log(`Alloc stress: ${elapsed}ms for ${iterations * 3} allocations (${allocsPerSec} allocs/sec)`);
