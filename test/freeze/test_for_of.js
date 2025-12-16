// Test for-of loop - should be freezable now
function sumArray(arr) {
    let sum = 0;
    for (let v of arr) {
        sum += v;
    }
    return sum;
}

const arr = [10, 20, 30, 40];
const result = sumArray(arr);
print('for-of sum:', result);  // Should print: for-of sum: 100
