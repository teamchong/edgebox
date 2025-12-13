// Allocator stress test - measures peak memory usage
// Memory measured externally via /usr/bin/time -l for fair comparison
// Uses simple JS features for Porffor compatibility

const ITERATIONS = 200000;  // 200k iterations = 600k objects

const objects = [];
for (let i = 0; i < ITERATIONS; i++) {
    // Simple object allocation (~64B)
    objects.push({ id: i, name: "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" });
    // Array allocation without .fill() for Porffor compatibility (~128B)
    const arr = [];
    for (let j = 0; j < 16; j++) arr.push(i);
    objects.push(arr);
    // String concatenation (~32B)
    objects.push("string-" + i);
}

// Verify and output count
if (objects.length !== ITERATIONS * 3) {
    console.log("FAIL: " + objects.length + " objects, expected " + (ITERATIONS * 3));
    if (typeof process !== 'undefined' && process.exit) process.exit(1);
} else {
    console.log(objects.length + " objects allocated");
}
