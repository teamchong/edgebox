// Test for-in loop - should be freezable now
function sumKeys(obj) {
    let sum = 0;
    for (let k in obj) {
        sum += parseInt(k) || 0;
    }
    return sum;
}

const obj = {1: 'a', 2: 'b', 3: 'c'};
const result = sumKeys(obj);
print('for-in sum:', result);  // Should print: for-in sum: 6
