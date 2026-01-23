// Test object literal with multiple properties
function testMultiProp() {
    const obj = { a: 1, b: 2, c: 3 };
    if (obj.a !== 1 || obj.b !== 2 || obj.c !== 3) {
        throw new Error("Multi-property object failed");
    }
}

// Test nested objects (like Zod schemas)
function testNestedObj() {
    const schema = {
        name: { type: "string" },
        age: { type: "number" }
    };
    if (schema.name.type !== "string" || schema.age.type !== "number") {
        throw new Error("Nested object failed");
    }
}

// Test function returning object with multiple properties
function testObjectReturn() {
    function createObj() {
        return { x: 10, y: 20, z: 30 };
    }
    const result = createObj();
    if (result.x !== 10 || result.y !== 20 || result.z !== 30) {
        throw new Error("Object return failed");
    }
}

// Test object with computed property values
function testComputedValues() {
    const val1 = 100;
    const val2 = 200;
    const obj = { first: val1, second: val2, sum: val1 + val2 };
    if (obj.first !== 100 || obj.second !== 200 || obj.sum !== 300) {
        throw new Error("Computed values failed");
    }
}

testMultiProp();
testNestedObj();
testObjectReturn();
testComputedValues();
console.log("PASS: define_field tests");
