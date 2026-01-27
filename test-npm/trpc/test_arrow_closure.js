console.log("=== Arrow Closure Test ===\n");

// Pattern 1: Outer returns async arrow function
function outer1() {
  return async (x) => {
    console.log("arrow1 called with", x);
    return "arrow1 " + x;
  };
}

// Pattern 2: Outer returns async function expression
function outer2() {
  return async function(x) {
    console.log("func2 called with", x);
    return "func2 " + x;
  };
}

// Pattern 3: Arrow function with captured variable
function outer3() {
  const captured = "captured";
  return async (x) => {
    console.log("arrow3 called, captured:", captured, "x:", x);
    return "arrow3 " + captured + " " + x;
  };
}

// Pattern 4: Function expression with captured variable
function outer4() {
  const captured = "captured";
  return async function(x) {
    console.log("func4 called, captured:", captured, "x:", x);
    return "func4 " + captured + " " + x;
  };
}

console.log("Pattern 1: arrow function");
const f1 = outer1();
const r1 = f1("test1");
console.log("r1 type:", typeof r1, "instanceof Promise:", r1 instanceof Promise);
if (r1 && r1.then) r1.then(v => console.log("r1 resolved:", v));

console.log("\nPattern 2: function expression");
const f2 = outer2();
const r2 = f2("test2");
console.log("r2 type:", typeof r2, "instanceof Promise:", r2 instanceof Promise);
if (r2 && r2.then) r2.then(v => console.log("r2 resolved:", v));

console.log("\nPattern 3: arrow with closure");
const f3 = outer3();
const r3 = f3("test3");
console.log("r3 type:", typeof r3, "instanceof Promise:", r3 instanceof Promise);
if (r3 && r3.then) r3.then(v => console.log("r3 resolved:", v));

console.log("\nPattern 4: function with closure");
const f4 = outer4();
const r4 = f4("test4");
console.log("r4 type:", typeof r4, "instanceof Promise:", r4 instanceof Promise);
if (r4 && r4.then) r4.then(v => console.log("r4 resolved:", v));

setTimeout(() => console.log("\n=== Done ==="), 100);
