//! Simple math WASM module for testing WASM imports
//! Provides basic arithmetic functions callable from JS

/// Add two 32-bit integers
export fn add(a: i32, b: i32) i32 {
    return a + b;
}

/// Multiply two 32-bit integers
export fn multiply(a: i32, b: i32) i32 {
    return a * b;
}

/// Subtract b from a
export fn subtract(a: i32, b: i32) i32 {
    return a - b;
}

/// Compute nth Fibonacci number (iterative)
export fn fibonacci(n: i32) i32 {
    if (n <= 1) return n;

    var prev: i32 = 0;
    var curr: i32 = 1;
    var i: i32 = 2;

    while (i <= n) : (i += 1) {
        const next = prev + curr;
        prev = curr;
        curr = next;
    }

    return curr;
}
