// Fibonacci benchmark - fib(45) for performance testing
function fib(n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

var start = Date.now();
var result = fib(45);
var elapsed = Date.now() - start;
print("fib(45) = " + result + " in " + elapsed + "ms");
