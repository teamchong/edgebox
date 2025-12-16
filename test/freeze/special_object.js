// Test special_object opcode - ALL types for freeze system

// Type 0: arguments object (strict mode)
function withArguments() {
    "use strict";
    return arguments.length;
}

function sumArguments() {
    "use strict";
    let total = 0;
    for (let i = 0; i < arguments.length; i++) {
        total += arguments[i];
    }
    return total;
}

// Type 1: mapped_arguments (sloppy mode with callee)
function sloppyWithCallee() {
    // In sloppy mode, arguments.callee is the function itself
    return arguments.callee.name;
}

// Type 2: this_func (via recursion)
function factorial(n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

// Test cases
print("=== Type 0: arguments (strict) ===");
print("withArguments(1,2,3) = " + withArguments(1, 2, 3));  // Expected: 3
print("sumArguments(1,2,3,4,5) = " + sumArguments(1, 2, 3, 4, 5));  // Expected: 15
print("sumArguments() = " + sumArguments());  // Expected: 0

print("\n=== Type 1: mapped_arguments (sloppy) ===");
print("sloppyWithCallee() = " + sloppyWithCallee());  // Expected: sloppyWithCallee

print("\n=== Type 2: this_func ===");
print("factorial(5) = " + factorial(5));  // Expected: 120

print("\n=== All tests passed! ===");
