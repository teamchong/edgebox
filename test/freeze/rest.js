// Test rest parameters for freeze system

function sum(...nums) {
    let total = 0;
    for (let i = 0; i < nums.length; i++) {
        total += nums[i];
    }
    return total;
}

function partial(a, b, ...rest) {
    return a + b + rest.length;
}

function first(...args) {
    return args[0];
}

// Test cases
print("sum(1,2,3,4,5) = " + sum(1, 2, 3, 4, 5));  // Expected: 15
print("partial(1,2,3,4,5) = " + partial(1, 2, 3, 4, 5));  // Expected: 6 (1+2+3)
print("first(42) = " + first(42));  // Expected: 42
print("sum() = " + sum());  // Expected: 0 (empty rest)
