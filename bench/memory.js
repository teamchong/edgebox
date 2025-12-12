// Allocator stress test - measures peak memory usage
// Memory measured externally via /usr/bin/time -l for fair comparison

const ITERATIONS = 200000;  // 200k iterations = 600k objects

const objects = [];
for (let i = 0; i < ITERATIONS; i++) {
    objects.push({ id: i, data: "x".repeat(32) });   // ~64B
    objects.push(new Array(16).fill(i));              // ~128B
    objects.push("string-" + i);                      // ~32B
}

// Verify and output count
if (objects.length !== ITERATIONS * 3) {
    console.log(`FAIL: ${objects.length} objects, expected ${ITERATIONS * 3}`);
    if (typeof process !== 'undefined' && process.exit) process.exit(1);
} else {
    console.log(`${objects.length} objects allocated`);
}
