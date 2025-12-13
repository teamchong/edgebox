function fib(n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

for (var i = 0; i <= 15; i++) {
    var result = fib(i);
    var expected = [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610][i];
    var status = result === expected ? "OK" : "FAIL";
    print("fib(" + i + ") = " + result + " (expected " + expected + ") " + status);
}
