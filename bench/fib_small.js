// Fibonacci benchmark - fib(10) for testing
function fib(n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

var result = fib(10);
print("fib(10) = " + result);
