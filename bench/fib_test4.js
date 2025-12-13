function fib(n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

print("Testing fib(45) once...");
var result1 = fib(45);
print("fib(45) = " + result1);

print("Testing fib(45) again...");
var result2 = fib(45);
print("fib(45) = " + result2);
