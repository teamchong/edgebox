// IO Benchmark - File Read Performance (Bun version)
// Tests reading a file multiple times

import { readFileSync } from 'fs';

const FILE_PATH = '/tmp/io_bench_test.txt';
const ITERATIONS = 1000;

// Warmup
for (let i = 0; i < 10; i++) {
    readFileSync(FILE_PATH, 'utf8');
}

const start = Date.now();
for (let i = 0; i < ITERATIONS; i++) {
    readFileSync(FILE_PATH, 'utf8');
}
const end = Date.now();

const elapsed = end - start;
const ops_per_sec = Math.round((ITERATIONS / elapsed) * 1000);

console.log('read_ops_per_sec:' + ops_per_sec);
