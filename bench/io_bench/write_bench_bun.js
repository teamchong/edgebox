// IO Benchmark - File Write Performance (Bun version)
// Tests writing a file multiple times

import { writeFileSync } from 'fs';

const FILE_PATH = '/tmp/io_bench_write_test.txt';
const ITERATIONS = 1000;
const DATA = 'Hello, World! This is a test file for IO benchmarking.\n'.repeat(100);

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
