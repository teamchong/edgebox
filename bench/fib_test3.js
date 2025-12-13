function fib(n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

print("Testing fib(45)...");
var result = fib(45);
print("fib(45) = " + result);
