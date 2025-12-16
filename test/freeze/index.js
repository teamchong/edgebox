// Test for-in loop
function sumKeys(obj) {
    let sum = 0;
    for (let k in obj) {
        sum += parseInt(k) || 0;
    }
    return sum;
}

// Test for-of loop
function sumArray(arr) {
    let sum = 0;
    for (let v of arr) {
        sum += v;
    }
    return sum;
}

// Test both
const obj = {1: 'a', 2: 'b', 3: 'c'};
const arr = [10, 20, 30, 40];

print('for-in sum:', sumKeys(obj));  // Should print 6
print('for-of sum:', sumArray(arr)); // Should print 100
print('TEST PASSED');
