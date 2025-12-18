// Test WASM imports functionality
// This test demonstrates loading and calling user WASM modules from JS

print("[test_wasm_import] Starting WASM import test...");
print("[test_wasm_import] Testing thin JS/rich Zig zero-copy pattern");

// Test 1: Load WASM module using __wasm_import()
try {
    const path = "./test_wasm/zig-out/bin/math.wasm";
    print(`[test_wasm_import] Loading WASM module: ${path}`);

    const math = __wasm_import(path);
    print("[test_wasm_import] ✅ Module loaded successfully");

    // Test 2: Call WASM functions
    print("\n[test_wasm_import] Testing WASM function calls:");

    const sum = math.add(5, 3);
    print(`  add(5, 3) = ${sum} (expected: 8)`);
    if (sum !== 8) throw new Error(`add() failed: got ${sum}, expected 8`);

    const product = math.multiply(4, 7);
    print(`  multiply(4, 7) = ${product} (expected: 28)`);
    if (product !== 28) throw new Error(`multiply() failed: got ${product}, expected 28`);

    const diff = math.subtract(10, 3);
    print(`  subtract(10, 3) = ${diff} (expected: 7)`);
    if (diff !== 7) throw new Error(`subtract() failed: got ${diff}, expected 7`);

    const fib = math.fibonacci(10);
    print(`  fibonacci(10) = ${fib} (expected: 55)`);
    if (fib !== 55) throw new Error(`fibonacci() failed: got ${fib}, expected 55`);

    print("\n[test_wasm_import] ✅ All function calls succeeded!");

    // Test 3: Benchmark performance (100k calls)
    print("\n[test_wasm_import] Running benchmark (100k calls)...");
    const start = Date.now();
    for (let i = 0; i < 100000; i++) {
        math.add(i, i + 1);
    }
    const elapsed = Date.now() - start;
    print(`[test_wasm_import] Benchmark: ${elapsed}ms for 100k calls`);
    print(`[test_wasm_import] Average: ${(elapsed * 1000000 / 100000).toFixed(0)}ns per call`);

    print("\n[test_wasm_import] ✅ All tests passed!");

} catch (e) {
    print(`[test_wasm_import] ❌ Error: ${e.message}`);
    print(e.stack);
}

print("[test_wasm_import] Test complete");
