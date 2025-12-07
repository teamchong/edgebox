// Hello World Example
// Run: ./run.sh examples/hello/index.js

print("Hello from EdgeBox!");
print("Platform:", os.platform());
print("Args:", JSON.stringify(scriptArgs));

// Test polyfills
const buf = Buffer.from("Hello");
print("Buffer:", buf.toString());

print("\nAll systems go!");
