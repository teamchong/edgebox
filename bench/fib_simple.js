// Simple fib for Porffor WASM - no loop, no timing
function fib(n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}
console.log(fib(45));
