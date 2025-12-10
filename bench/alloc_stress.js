// Allocator stress test - measures malloc/free performance
// Verifies correctness by checking final state

const iterations = 10000;
const objects = [];

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

// Verify correctness - last batch should have 3000 items (iterations 9001-10000 * 3)
const expectedLen = (iterations % 1000) * 3; // 0 because 10000 % 1000 = 0, cleared
const actualLen = objects.length;

if (actualLen !== expectedLen) {
    console.log(`FAIL: objects.length = ${actualLen}, expected ${expectedLen}`);
    if (typeof process !== 'undefined' && process.exit) process.exit(1);
} else {
    console.log(`OK: ${iterations * 3} allocations completed`);
}
