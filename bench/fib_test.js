function fib(n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

print("fib(20) = " + fib(20));
print("fib(25) = " + fib(25));
print("fib(30) = " + fib(30));
