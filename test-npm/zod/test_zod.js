const { z } = require('zod');

console.log("=== Zod Validation Test ===\n");

let passed = 0;
let failed = 0;

function test(name, fn) {
    try {
        fn();
        console.log("[PASS] " + name);
        passed++;
    } catch (e) {
        console.log("[FAIL] " + name + ": " + e.message);
        failed++;
    }
}

// Test 1: String validation
test("String validation", () => {
    const schema = z.string();
    const result = schema.parse("hello");
    if (result !== "hello") throw new Error("Expected 'hello'");
});

// Test 2: Number validation
test("Number validation", () => {
    const schema = z.number();
    const result = schema.parse(42);
    if (result !== 42) throw new Error("Expected 42");
});

// Test 3: Object schema
test("Object schema", () => {
    const User = z.object({
        name: z.string(),
        age: z.number()
    });
    const result = User.parse({ name: "Alice", age: 30 });
    if (result.name !== "Alice" || result.age !== 30) throw new Error("Parse failed");
});

// Test 4: Invalid input throws
test("Invalid input throws", () => {
    const schema = z.string();
    try {
        schema.parse(123);
        throw new Error("Should have thrown");
    } catch (e) {
        // ZodError has either .errors array or .issues array
        const hasZodError = e.errors || e.issues || e.name === 'ZodError';
        if (!hasZodError && !e.message.includes("Expected string")) {
            throw new Error("Expected ZodError, got: " + e.name + " - " + e.message);
        }
        // Successfully caught the validation error
    }
});

// Test 5: Optional fields
test("Optional fields", () => {
    const schema = z.object({
        name: z.string(),
        email: z.string().optional()
    });
    const result = schema.parse({ name: "Bob" });
    if (result.name !== "Bob") throw new Error("Parse failed");
});

// Test 6: Array validation
test("Array validation", () => {
    const schema = z.array(z.number());
    const result = schema.parse([1, 2, 3]);
    if (result.length !== 3) throw new Error("Expected array of 3");
});

// Test 7: Union types
test("Union types", () => {
    const schema = z.union([z.string(), z.number()]);
    const str = schema.parse("hello");
    const num = schema.parse(42);
    if (str !== "hello" || num !== 42) throw new Error("Union parse failed");
});

// Test 8: Default values
test("Default values", () => {
    const schema = z.string().default("default");
    const result = schema.parse(undefined);
    if (result !== "default") throw new Error("Expected default value");
});

// Test 9: Transform
test("Transform", () => {
    const schema = z.string().transform(s => s.toUpperCase());
    const result = schema.parse("hello");
    if (result !== "HELLO") throw new Error("Transform failed");
});

// Test 10: Enum
test("Enum", () => {
    const Status = z.enum(["pending", "done", "failed"]);
    const result = Status.parse("done");
    if (result !== "done") throw new Error("Enum parse failed");
});

// Summary
console.log("\n=== Summary ===");
console.log("Passed: " + passed);
console.log("Failed: " + failed);
console.log("Result: " + (failed === 0 ? "ALL PASS" : "SOME FAILED"));
