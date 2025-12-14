function fib(n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

function ack(m, n) {
    if (m === 0) return n + 1;
    if (n === 0) return ack(m - 1, 1);
    return ack(m - 1, ack(m, n - 1));
}

print("fib(10) = " + fib(10));
print("fib(20) = " + fib(20));
print("ack(2,3) = " + ack(2,3));
print("ack(3,3) = " + ack(3,3));
