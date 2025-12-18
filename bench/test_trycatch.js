// Test try-catch with block-level fallback
// Expected: Clean blocks (before try) execute in frozen C (18x faster)
//           Contaminated blocks (try-catch) fall back to interpreter

function testPartialFreeze(n) {
    // Block 0-1: Clean blocks (should be frozen)
    let sum = 0;
    for (let i = 0; i < n; i++) {
        sum += i;
    }

    // Block 2: Contaminated block (try-catch, should fall back to interpreter)
    try {
        if (sum > 100) {
            throw new Error("Sum too large: " + sum);
        }
        return sum;
    } catch (e) {
        print("Caught error:", e.message);
        return -1;
    }
}

// Test 1: Normal case (no exception)
const result1 = testPartialFreeze(10);
print("Test 1 (n=10):", result1);  // Should print 45

// Test 2: Exception case
const result2 = testPartialFreeze(20);
print("Test 2 (n=20):", result2);  // Should catch error and print -1

// Benchmark: Run many iterations to see frozen speedup
const start = Date.now();
for (let i = 0; i < 100000; i++) {
    testPartialFreeze(10);
}
const elapsed = Date.now() - start;
print("Benchmark (100k iterations):", elapsed + "ms");
