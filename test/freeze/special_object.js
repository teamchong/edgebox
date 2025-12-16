// Test special_object opcode types for freeze system

// Type 0: arguments object
function withArguments() {
    return arguments.length;
}

function sumArguments() {
    let total = 0;
    for (let i = 0; i < arguments.length; i++) {
        total += arguments[i];
    }
    return total;
}

// Test cases
print("withArguments(1,2,3) = " + withArguments(1, 2, 3));  // Expected: 3
print("sumArguments(1,2,3,4,5) = " + sumArguments(1, 2, 3, 4, 5));  // Expected: 15
print("sumArguments() = " + sumArguments());  // Expected: 0
