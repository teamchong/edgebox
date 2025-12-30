// IO Benchmark - File Write Performance (Node.js version)
// Tests writing a file multiple times

const { writeFileSync } = require('fs');

const FILE_PATH = '/tmp/io_bench_write_test.txt';
const ITERATIONS = 1000;
// 64-byte base string repeated 64 times = 4096 bytes (4KB) - matches EdgeBox version
const DATA = 'Hello, World! This is a test file for IO benchmarking.!!!!!!!!\n'.repeat(64);

// Warmup
for (let i = 0; i < 10; i++) {
    writeFileSync(FILE_PATH, DATA);
}

const start = Date.now();
for (let i = 0; i < ITERATIONS; i++) {
    writeFileSync(FILE_PATH, DATA);
}
const end = Date.now();

const elapsed = end - start;
const ops_per_sec = Math.round((ITERATIONS / elapsed) * 1000);

console.log('write_ops_per_sec:' + ops_per_sec);
