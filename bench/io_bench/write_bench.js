// IO Benchmark - File Write Performance
// Tests writing a file multiple times

var fs = require('fs');

var FILE_PATH = '/tmp/io_bench_write_test.txt';
var ITERATIONS = 1000;
var DATA = 'Hello, World! This is a test file for IO benchmarking.\n';
for (var j = 0; j < 6; j++) DATA = DATA + DATA; // ~3.5KB

// Warmup
for (var i = 0; i < 10; i++) {
    fs.writeFileSync(FILE_PATH, DATA);
}

var start = Date.now();
for (var i = 0; i < ITERATIONS; i++) {
    fs.writeFileSync(FILE_PATH, DATA);
}
var end = Date.now();

var elapsed = end - start;
var ops_per_sec = Math.round((ITERATIONS / elapsed) * 1000);

console.log('write_ops_per_sec:' + ops_per_sec);
