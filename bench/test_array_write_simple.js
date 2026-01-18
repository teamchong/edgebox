// Minimal test for array write in loop
function writeLoop(arr, len) {
    for (var i = 0; i < len; i++) {
        arr[i] = i;
    }
    return arr[0];
}

var data = new Int32Array(10);
var result = writeLoop(data, 10);
print(result === 0 ? "PASS" : "FAIL: " + result);
