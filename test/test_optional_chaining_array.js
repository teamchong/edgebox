// Test optional chaining with computed property access
// This pattern generates DUP in vstack mode which can cause
// vstack/real-stack mismatch at control flow boundaries

function testOptionalArrayAccess() {
    const obj = { shape: { name: "string", age: "number" } };
    const keys = ["name", "age"];

    for (const k of keys) {
        // This pattern: obj.shape?.[k]
        // Generates: get_field "shape", DUP, is_undefined_or_null, if_false, get_array_el
        const val = obj.shape?.[k];
        if (val !== obj.shape[k]) {
            throw new Error("Optional chaining failed for key: " + k);
        }
    }
}

// Test with undefined shape
function testOptionalWithUndefined() {
    const obj = {};
    const k = "name";
    const val = obj.shape?.[k];
    if (val !== undefined) {
        throw new Error("Expected undefined for missing shape");
    }
}

// Test nested optional chaining with computed access
function testNestedOptionalChaining() {
    const data = {
        users: {
            alice: { name: "Alice", age: 30 },
            bob: { name: "Bob", age: 25 }
        }
    };

    const userKeys = ["alice", "bob", "charlie"];
    const results = [];

    for (const key of userKeys) {
        // Optional chaining with computed property in a loop
        const user = data.users?.[key];
        results.push(user?.name ?? "unknown");
    }

    if (results[0] !== "Alice") throw new Error("Expected Alice, got " + results[0]);
    if (results[1] !== "Bob") throw new Error("Expected Bob, got " + results[1]);
    if (results[2] !== "unknown") throw new Error("Expected unknown, got " + results[2]);
}

// Test optional chaining with method calls
function testOptionalMethodCall() {
    const obj = {
        methods: {
            greet: () => "hello",
            farewell: () => "goodbye"
        }
    };

    const methodNames = ["greet", "farewell", "missing"];

    for (const name of methodNames) {
        const method = obj.methods?.[name];
        const result = method?.();
        if (name === "greet" && result !== "hello") {
            throw new Error("Expected hello, got " + result);
        }
        if (name === "farewell" && result !== "goodbye") {
            throw new Error("Expected goodbye, got " + result);
        }
        if (name === "missing" && result !== undefined) {
            throw new Error("Expected undefined for missing method, got " + result);
        }
    }
}

// Run all tests
testOptionalArrayAccess();
testOptionalWithUndefined();
testNestedOptionalChaining();
testOptionalMethodCall();

console.log("PASS: optional chaining array access tests");
