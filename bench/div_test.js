// Division test
var log = typeof print === "function" ? print : console.log;

function divide(a, b) {
    return a / b;
}

var result = divide(10, 2);
log("divide(10, 2) = " + result + " (expected 5)");

result = divide(100, 4);
log("divide(100, 4) = " + result + " (expected 25)");

result = divide(9, 2);
log("divide(9, 2) = " + result + " (expected 4.5)");
