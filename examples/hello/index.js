// Hello World Example
// Run: ./run.sh examples/hello/index.js

console.log("Hello from EdgeBox!");
console.log("Platform:", process.platform);
console.log("Args:", JSON.stringify(process.argv));

// Test polyfills
const buf = Buffer.from("Hello");
console.log("Buffer:", buf.toString());

console.log("\nAll systems go!");
